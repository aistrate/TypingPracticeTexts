module Simplify ( simplTopBinds, simplExpr ) where

#include "HsVersions.h"

import DynFlags
import SimplMonad
import Type hiding      ( substTy, extendTvSubst, substTyVar )
import SimplEnv
import SimplUtils
import FamInstEnv	( FamInstEnv )
import Id
import MkId		( seqId, realWorldPrimId )
import MkCore		( mkImpossibleExpr )
import Var
import IdInfo
import Name		( mkSystemVarName, isExternalName )
import Coercion
import OptCoercion	( optCoercion )
import FamInstEnv       ( topNormaliseType )
import DataCon          ( DataCon, dataConWorkId, dataConRepStrictness )
import CoreMonad        ( Tick(..), SimplifierMode(..) )
import CoreSyn
import Demand           ( isStrictDmd )
import PprCore          ( pprParendExpr, pprCoreExpr )
import CoreUnfold 
import CoreUtils
import qualified CoreSubst
import CoreArity
import Rules            ( lookupRule, getRules )
import BasicTypes       ( isMarkedStrict, Arity )
import CostCentre       ( currentCCS, pushCCisNop )
import TysPrim          ( realWorldStatePrimTy )
import BasicTypes       ( TopLevelFlag(..), isTopLevel, RecFlag(..) )
import MonadUtils	( foldlM, mapAccumLM )
import Maybes           ( orElse )
import Data.List        ( mapAccumL )
import Outputable
import FastString
simplTopBinds :: SimplEnv -> [InBind] -> SimplM SimplEnv

simplTopBinds env0 binds0
  = do  {
        ; env1 <- simplRecBndrs env0 (bindersOfBinds binds0)
        ; dflags <- getDOptsSmpl
        ; let dump_flag = dopt Opt_D_verbose_core2core dflags
        ; env2 <- simpl_binds dump_flag env1 binds0
        ; freeTick SimplifierDone
        ; return env2 }
  where
    simpl_binds :: Bool -> SimplEnv -> [InBind] -> SimplM SimplEnv
    simpl_binds _    env []           = return env
    simpl_binds dump env (bind:binds) = do { env' <- trace_bind dump bind $
                                                     simpl_bind env bind
                                           ; simpl_binds dump env' binds }

    trace_bind True  bind = pprTrace "SimplBind" (ppr (bindersOf bind))
    trace_bind False _    = \x -> x

    simpl_bind env (Rec pairs)  = simplRecBind      env  TopLevel pairs
    simpl_bind env (NonRec b r) = simplRecOrTopPair env' TopLevel NonRecursive b b' r
        where
          (env', b') = addBndrRules env b (lookupRecBndr env b)
simplRecBind :: SimplEnv -> TopLevelFlag
             -> [(InId, InExpr)]
             -> SimplM SimplEnv
simplRecBind env0 top_lvl pairs0
  = do  { let (env_with_info, triples) = mapAccumL add_rules env0 pairs0
        ; env1 <- go (zapFloats env_with_info) triples
        ; return (env0 `addRecFloats` env1) }
  where
    add_rules :: SimplEnv -> (InBndr,InExpr) -> (SimplEnv, (InBndr, OutBndr, InExpr))
    add_rules env (bndr, rhs) = (env', (bndr, bndr', rhs))
        where
          (env', bndr') = addBndrRules env bndr (lookupRecBndr env bndr)

    go env [] = return env

    go env ((old_bndr, new_bndr, rhs) : pairs)
        = do { env' <- simplRecOrTopPair env top_lvl Recursive old_bndr new_bndr rhs
             ; go env' pairs }
simplRecOrTopPair :: SimplEnv
                  -> TopLevelFlag -> RecFlag
                  -> InId -> OutBndr -> InExpr
                  -> SimplM SimplEnv

simplRecOrTopPair env top_lvl is_rec old_bndr new_bndr rhs
  | preInlineUnconditionally env top_lvl old_bndr rhs
  = do  { tick (PreInlineUnconditionally old_bndr)
        ; return (extendIdSubst env old_bndr (mkContEx env rhs)) }

  | otherwise
  = simplLazyBind env top_lvl is_rec old_bndr new_bndr rhs env
simplLazyBind :: SimplEnv
              -> TopLevelFlag -> RecFlag
              -> InId -> OutId
              -> InExpr -> SimplEnv
              -> SimplM SimplEnv

simplLazyBind env top_lvl is_rec bndr bndr1 rhs rhs_se
  =
    do  { let   rhs_env     = rhs_se `setInScope` env
		(tvs, body) = case collectTyBinders rhs of
			        (tvs, body) | not_lam body -> (tvs,body)
					    | otherwise	   -> ([], rhs)
		not_lam (Lam _ _) = False
		not_lam _	  = True

        ; (body_env, tvs') <- simplBinders rhs_env tvs

        ; (body_env1, body1) <- simplExprF body_env body mkRhsStop
        ; (body_env2, body2) <- prepareRhs top_lvl body_env1 bndr1 body1

        ; (env', rhs')
            <-  if not (doFloatFromRhs top_lvl is_rec False body2 body_env2)
                then
                     do { rhs' <- mkLam env tvs' (wrapFloats body_env1 body1)
                        ; return (env, rhs') }

                else if null tvs then
                     do { tick LetFloatFromLet
                        ; return (addFloats env body_env2, body2) }

                else
                     do { tick LetFloatFromLet
                        ; (poly_binds, body3) <- abstractFloats tvs' body_env2 body2
                        ; rhs' <- mkLam env tvs' body3
                        ; env' <- foldlM (addPolyBind top_lvl) env poly_binds
                        ; return (env', rhs') }

        ; completeBind env' top_lvl bndr bndr1 rhs' }
simplNonRecX :: SimplEnv
             -> InId
             -> OutExpr
             -> SimplM SimplEnv

simplNonRecX env bndr new_rhs
  | isDeadBinder bndr
  = return env
  | otherwise
  = do  { (env', bndr') <- simplBinder env bndr
        ; completeNonRecX NotTopLevel env' (isStrictId bndr) bndr bndr' new_rhs }

completeNonRecX :: TopLevelFlag -> SimplEnv
                -> Bool
                -> InId
                -> OutId
                -> OutExpr
                -> SimplM SimplEnv

completeNonRecX top_lvl env is_strict old_bndr new_bndr new_rhs
  = do  { (env1, rhs1) <- prepareRhs top_lvl (zapFloats env) new_bndr new_rhs
        ; (env2, rhs2) <- 
                if doFloatFromRhs NotTopLevel NonRecursive is_strict rhs1 env1
                then do { tick LetFloatFromLet
                        ; return (addFloats env env1, rhs1) }
                else return (env, wrapFloats env1 rhs1)
        ; completeBind env2 NotTopLevel old_bndr new_bndr rhs2 }
prepareRhs :: TopLevelFlag -> SimplEnv -> OutId -> OutExpr -> SimplM (SimplEnv, OutExpr)
prepareRhs top_lvl env id (Cast rhs co)
  | (ty1, _ty2) <- coercionKind co
  , not (isUnLiftedType ty1)
  = do  { (env', rhs') <- makeTrivialWithInfo top_lvl env sanitised_info rhs
        ; return (env', Cast rhs' co) }
  where
    sanitised_info = vanillaIdInfo `setStrictnessInfo` strictnessInfo info
                                   `setDemandInfo`     demandInfo info
    info = idInfo id

prepareRhs top_lvl env0 _ rhs0
  = do  { (_is_exp, env1, rhs1) <- go 0 env0 rhs0
        ; return (env1, rhs1) }
  where
    go n_val_args env (Cast rhs co)
        = do { (is_exp, env', rhs') <- go n_val_args env rhs
             ; return (is_exp, env', Cast rhs' co) }
    go n_val_args env (App fun (Type ty))
        = do { (is_exp, env', rhs') <- go n_val_args env fun
             ; return (is_exp, env', App rhs' (Type ty)) }
    go n_val_args env (App fun arg)
        = do { (is_exp, env', fun') <- go (n_val_args+1) env fun
             ; case is_exp of
                True -> do { (env'', arg') <- makeTrivial top_lvl env' arg
                           ; return (True, env'', App fun' arg') }
                False -> return (False, env, App fun arg) }
    go n_val_args env (Var fun)
        = return (is_exp, env, Var fun)
        where
          is_exp = isExpandableApp fun n_val_args

    go _ env other
        = return (False, env, other)
makeTrivial :: TopLevelFlag -> SimplEnv -> OutExpr -> SimplM (SimplEnv, OutExpr)
makeTrivial top_lvl env expr = makeTrivialWithInfo top_lvl env vanillaIdInfo expr

makeTrivialWithInfo :: TopLevelFlag -> SimplEnv -> IdInfo 
                    -> OutExpr -> SimplM (SimplEnv, OutExpr)
makeTrivialWithInfo top_lvl env info expr
  | exprIsTrivial expr
  || not (bindingOk top_lvl expr expr_ty)
  = return (env, expr)
  | otherwise
  = do  { uniq <- getUniqueM
        ; let name = mkSystemVarName uniq (fsLit "a")
              var = mkLocalIdWithInfo name expr_ty info
        ; env'  <- completeNonRecX top_lvl env False var var expr
	; expr' <- simplVar env' var
        ; return (env', expr') }
  where
    expr_ty = exprType expr

bindingOk :: TopLevelFlag -> CoreExpr -> Type -> Bool
bindingOk top_lvl _ expr_ty
  | isTopLevel top_lvl = not (isUnLiftedType expr_ty) 
  | otherwise          = True
completeBind :: SimplEnv
             -> TopLevelFlag
             -> InId
             -> OutId -> OutExpr
             -> SimplM SimplEnv

completeBind env top_lvl old_bndr new_bndr new_rhs
 = ASSERT( isId new_bndr )
   do { let old_info = idInfo old_bndr
	    old_unf  = unfoldingInfo old_info
	    occ_info = occInfo old_info

      ; (new_arity, final_rhs) <- tryEtaExpand env new_bndr new_rhs

      ; new_unfolding <- simplUnfolding env top_lvl old_bndr final_rhs old_unf

      ; if postInlineUnconditionally env top_lvl new_bndr occ_info final_rhs new_unfolding
	then do  { tick (PostInlineUnconditionally old_bndr)
	         ;
                   return (extendIdSubst env old_bndr (DoneEx final_rhs)) }
	else
   do { let info1 = idInfo new_bndr `setArityInfo` new_arity
	
	    info2 = info1 `setUnfoldingInfo` new_unfolding

            info3 | isEvaldUnfolding new_unfolding = zapDemandInfo info2 `orElse` info2
                  | otherwise                      = info2

            final_id = new_bndr `setIdInfo` info3

      ;
        return (addNonRec env final_id final_rhs) } }

addPolyBind :: TopLevelFlag -> SimplEnv -> OutBind -> SimplM SimplEnv

addPolyBind top_lvl env (NonRec poly_id rhs)
  = do  { unfolding <- simplUnfolding env top_lvl poly_id rhs noUnfolding
        ; let final_id = setIdInfo poly_id $
                         idInfo poly_id `setUnfoldingInfo` unfolding
                                        `setArityInfo`     exprArity rhs

        ; return (addNonRec env final_id rhs) }

addPolyBind _ env bind@(Rec _) 
  = return (extendFloats env bind)

simplUnfolding :: SimplEnv-> TopLevelFlag
               -> InId
               -> OutExpr
	       -> Unfolding -> SimplM Unfolding
simplUnfolding env _ _ _ (DFunUnfolding ar con ops)
  = return (DFunUnfolding ar con ops')
  where
    ops' = map (fmap (substExpr (text "simplUnfolding") env)) ops

simplUnfolding env top_lvl id _
    (CoreUnfolding { uf_tmpl = expr, uf_arity = arity
                   , uf_src = src, uf_guidance = guide })
  | isStableSource src
  = do { expr' <- simplExpr rule_env expr
       ; let src' = CoreSubst.substUnfoldingSource (mkCoreSubst (text "inline-unf") env) src
             is_top_lvl = isTopLevel top_lvl
       ; case guide of
           UnfWhen sat_ok _
              -> let guide' = UnfWhen sat_ok (inlineBoringOk expr')
                 in return (mkCoreUnfolding src' is_top_lvl expr' arity guide')

           _other
              -> let bottoming = isBottomingId id
                 in bottoming `seq`
                    return (mkUnfolding src' is_top_lvl bottoming expr')
       }
  where
    act      = idInlineActivation id
    rule_env = updMode (updModeForInlineRules act) env

simplUnfolding _ top_lvl id new_rhs _
  = let bottoming = isBottomingId id
    in bottoming `seq`
       return (mkUnfolding InlineRhs (isTopLevel top_lvl) bottoming new_rhs)
simplExpr :: SimplEnv -> CoreExpr -> SimplM CoreExpr
simplExpr env expr = simplExprC env expr mkBoringStop

simplExprC :: SimplEnv -> CoreExpr -> SimplCont -> SimplM CoreExpr
simplExprC env expr cont
  =
    do  { (env', expr') <- simplExprF (zapFloats env) expr cont
        ;
          return (wrapFloats env' expr') }

simplExprF :: SimplEnv -> InExpr -> SimplCont
           -> SimplM (SimplEnv, OutExpr)

simplExprF env e cont
  =
    simplExprF' env e cont

simplExprF' :: SimplEnv -> InExpr -> SimplCont
            -> SimplM (SimplEnv, OutExpr)
simplExprF' env (Var v)        cont = simplVarF env v cont
simplExprF' env (Lit lit)      cont = rebuild env (Lit lit) cont
simplExprF' env (Note n expr)  cont = simplNote env n expr cont
simplExprF' env (Cast body co) cont = simplCast env body co cont
simplExprF' env (App fun arg)  cont = simplExprF env fun $
                                      ApplyTo NoDup arg env cont

simplExprF' env expr@(Lam _ _) cont
  = simplLam env zapped_bndrs body cont
  where
    (bndrs, body) = collectBinders expr
    zapped_bndrs | need_to_zap = map zap bndrs
                 | otherwise   = bndrs

    need_to_zap = any zappable_bndr (drop n_args bndrs)
    n_args = countArgs cont
        
    zappable_bndr b = isId b && not (isOneShotBndr b)
    zap b | isTyCoVar b = b
          | otherwise   = zapLamIdInfo b

simplExprF' env (Type ty) cont
  = ASSERT( contIsRhsOrArg cont )
    do  { ty' <- simplCoercion env ty
        ; rebuild env (Type ty') cont }

simplExprF' env (Case scrut bndr _ alts) cont
  | sm_case_case (getMode env)
  =
    simplExprF env scrut (Select NoDup bndr alts env cont)

  | otherwise
  =
    do  { case_expr' <- simplExprC env scrut
                             (Select NoDup bndr alts env mkBoringStop)
        ; rebuild env case_expr' cont }

simplExprF' env (Let (Rec pairs) body) cont
  = do  { env' <- simplRecBndrs env (map fst pairs)

        ; env'' <- simplRecBind env' NotTopLevel pairs
        ; simplExprF env'' body cont }

simplExprF' env (Let (NonRec bndr rhs) body) cont
  = simplNonRecE env bndr (rhs, env) ([], body) cont

simplType :: SimplEnv -> InType -> SimplM OutType
simplType env ty
  =
    seqType new_ty `seq` return new_ty
  where
    new_ty = substTy env ty

simplCoercion :: SimplEnv -> InType -> SimplM OutType
simplCoercion env co
  = seqType new_co `seq` return new_co
  where 
    new_co = optCoercion (getTvSubst env) co
rebuild :: SimplEnv -> OutExpr -> SimplCont -> SimplM (SimplEnv, OutExpr)
rebuild env expr cont
  = case cont of
      Stop {}                      -> return (env, expr)
      CoerceIt co cont             -> rebuild env (mkCoerce co expr) cont
      Select _ bndr alts se cont   -> rebuildCase (se `setFloats` env) expr bndr alts cont
      StrictArg info _ cont        -> rebuildCall env (info `addArgTo` expr) cont
      StrictBind b bs body se cont -> do { env' <- simplNonRecX (se `setFloats` env) b expr
                                         ; simplLam env' bs body cont }
      ApplyTo dup_flag arg se cont
        | isSimplified dup_flag    -> rebuild env (App expr arg) cont
        | otherwise                -> do { arg' <- simplExpr (se `setInScope` env) arg
                                         ; rebuild env (App expr arg') cont }
simplCast :: SimplEnv -> InExpr -> Coercion -> SimplCont
          -> SimplM (SimplEnv, OutExpr)
simplCast env body co0 cont0
  = do  { co1 <- simplCoercion env co0
        ; simplExprF env body (addCoerce co1 cont0) }
  where
       addCoerce co cont = add_coerce co (coercionKind co) cont

       add_coerce _co (s1, k1) cont
         | s1 `coreEqType` k1 = cont

       add_coerce co1 (s1, _k2) (CoerceIt co2 cont)
         | (_l1, t1) <- coercionKind co2
         , s1 `coreEqType` t1  = cont
         | otherwise           = CoerceIt (mkTransCoercion co1 co2) cont

       add_coerce co (s1s2, _t1t2) (ApplyTo dup (Type arg_ty) arg_se cont)
         | Just (tyvar,_) <- splitForAllTy_maybe s1s2
         = let 
             (new_arg_ty, new_cast)
               | isCoVar tyvar = (new_arg_co, mkCselRCoercion co)
               | otherwise     = (ty',        mkInstCoercion co ty')
           in 
           ApplyTo dup (Type new_arg_ty) (zapSubstEnv arg_se) (addCoerce new_cast cont)
         where
           ty' = substTy (arg_se `setInScope` env) arg_ty
	   new_arg_co = mkCsel1Coercion co  `mkTransCoercion`
                              ty'           `mkTransCoercion`
                        mkSymCoercion (mkCsel2Coercion co)

       add_coerce co (s1s2, _t1t2) (ApplyTo dup arg arg_se cont)
         | not (isTypeArg arg)
         , isFunTy s1s2
         = ApplyTo dup new_arg (zapSubstEnv arg_se) (addCoerce co2 cont)
         where
           [co1, co2] = decomposeCo 2 co
           new_arg    = mkCoerce (mkSymCoercion co1) arg'
           arg'       = substExpr (text "move-cast") (arg_se `setInScope` env) arg

       add_coerce co _ cont = CoerceIt co cont
simplLam :: SimplEnv -> [InId] -> InExpr -> SimplCont
         -> SimplM (SimplEnv, OutExpr)

simplLam env [] body cont = simplExprF env body cont

simplLam env (bndr:bndrs) body (ApplyTo _ arg arg_se cont)
  = do  { tick (BetaReduction bndr)
        ; simplNonRecE env (zap_unfolding bndr) (arg, arg_se) (bndrs, body) cont }
  where
    zap_unfolding bndr
      | isId bndr, isStableUnfolding (realIdUnfolding bndr)
      = setIdUnfolding bndr NoUnfolding
      | otherwise = bndr

simplLam env bndrs body cont
  = do  { (env', bndrs') <- simplLamBndrs env bndrs
        ; body' <- simplExpr env' body
        ; new_lam <- mkLam env' bndrs' body'
        ; rebuild env' new_lam cont }

simplNonRecE :: SimplEnv
             -> InBndr
             -> (InExpr, SimplEnv)
             -> ([InBndr], InExpr)
             -> SimplCont
             -> SimplM (SimplEnv, OutExpr)

simplNonRecE env bndr (Type ty_arg, rhs_se) (bndrs, body) cont
  = ASSERT( isTyCoVar bndr )
    do	{ ty_arg' <- simplType (rhs_se `setInScope` env) ty_arg
	; simplLam (extendTvSubst env bndr ty_arg') bndrs body cont }

simplNonRecE env bndr (rhs, rhs_se) (bndrs, body) cont
  | preInlineUnconditionally env NotTopLevel bndr rhs
  = do  { tick (PreInlineUnconditionally bndr)
        ;
          simplLam (extendIdSubst env bndr (mkContEx rhs_se rhs)) bndrs body cont }

  | isStrictId bndr
  = do  { simplExprF (rhs_se `setFloats` env) rhs
                     (StrictBind bndr bndrs body env cont) }

  | otherwise
  = ASSERT( not (isTyCoVar bndr) )
    do  { (env1, bndr1) <- simplNonRecBndr env bndr
        ; let (env2, bndr2) = addBndrRules env1 bndr bndr1
        ; env3 <- simplLazyBind env2 NotTopLevel NonRecursive bndr bndr2 rhs rhs_se
        ; simplLam env3 bndrs body cont }
simplNote :: SimplEnv -> Note -> CoreExpr -> SimplCont
          -> SimplM (SimplEnv, OutExpr)
simplNote env (SCC cc) e cont
  | pushCCisNop cc (getEnclosingCC env)
  = simplExprF env e cont
  | otherwise
  = do  { e' <- simplExpr (setEnclosingCC env currentCCS) e
        ; rebuild env (mkSCC cc e') cont }

simplNote env (CoreNote s) e cont
  = do { e' <- simplExpr env e
       ; rebuild env (Note (CoreNote s) e') cont }
simplVar :: SimplEnv -> InVar -> SimplM OutExpr
simplVar env var
  | isTyCoVar var 
  = return (Type (substTyVar env var))
  | otherwise
  = case substId env var of
        DoneId var1      -> return (Var var1)
        DoneEx e         -> return e
        ContEx tvs ids e -> simplExpr (setSubstEnv env tvs ids) e

simplVarF :: SimplEnv -> InId -> SimplCont -> SimplM (SimplEnv, OutExpr)
simplVarF env var cont
  = case substId env var of
        DoneEx e         -> simplExprF (zapSubstEnv env) e cont
        ContEx tvs ids e -> simplExprF (setSubstEnv env tvs ids) e cont
        DoneId var1      -> completeCall env var1 cont

completeCall :: SimplEnv -> Id -> SimplCont -> SimplM (SimplEnv, OutExpr)
completeCall env var cont
  = do  {   ------------- Try inlining --------------
          dflags <- getDOptsSmpl
        ; let  (lone_variable, arg_infos, call_cont) = contArgs cont

               n_val_args = length arg_infos
               interesting_cont = interestingCallContext call_cont
               unfolding    = activeUnfolding env var
               maybe_inline = callSiteInline dflags var unfolding
                                             lone_variable arg_infos interesting_cont
        ; case maybe_inline of {
            Just expr
              ->  do { tick (UnfoldingDone var)
                     ; trace_inline dflags expr cont $
                       simplExprF (zapSubstEnv env) expr cont }

            ; Nothing -> do

        { rule_base <- getSimplRules
        ; let info = mkArgInfo var (getRules rule_base var) n_val_args call_cont
        ; rebuildCall env info cont
    }}}
  where
    trace_inline dflags unfolding cont stuff
      | not (dopt Opt_D_dump_inlinings dflags) = stuff
      | not (dopt Opt_D_verbose_core2core dflags) 
      = if isExternalName (idName var) then 
      	  pprTrace "Inlining done:" (ppr var) stuff
        else stuff
      | otherwise
      = pprTrace ("Inlining done: " ++ showSDoc (ppr var))
           (vcat [text "Inlined fn: " <+> nest 2 (ppr unfolding),
                  text "Cont:  " <+> ppr cont])
           stuff

rebuildCall :: SimplEnv
            -> ArgInfo
            -> SimplCont
            -> SimplM (SimplEnv, OutExpr)
rebuildCall env (ArgInfo { ai_fun = fun, ai_args = rev_args, ai_strs = [] }) cont
  | not (contIsTrivial cont)
  = return (env, mk_coerce res)
  where
    res     = mkApps (Var fun) (reverse rev_args)
    res_ty  = exprType res
    cont_ty = contResultType env res_ty cont
    co      = mkUnsafeCoercion res_ty cont_ty
    mk_coerce expr | cont_ty `coreEqType` res_ty = expr
                   | otherwise = mkCoerce co expr

rebuildCall env info (ApplyTo _ (Type arg_ty) se cont)
  = do  { ty' <- simplCoercion (se `setInScope` env) arg_ty
        ; rebuildCall env (info `addArgTo` Type ty') cont }

rebuildCall env info@(ArgInfo { ai_encl = encl_rules
                              , ai_strs = str:strs, ai_discs = disc:discs })
            (ApplyTo dup_flag arg arg_se cont)
  | isSimplified dup_flag
  = rebuildCall env (addArgTo info' arg) cont

  | str
  =
    simplExprF (arg_se `setFloats` env) arg
               (StrictArg info' cci cont)

  | otherwise
  = do  { arg' <- simplExprC (arg_se `setInScope` env) arg
                             (mkLazyArgStop cci)
        ; rebuildCall env (addArgTo info' arg') cont }
  where
    info' = info { ai_strs = strs, ai_discs = discs }
    cci | encl_rules || disc > 0 = ArgCtxt encl_rules
        | otherwise              = BoringCtxt

rebuildCall env (ArgInfo { ai_fun = fun, ai_args = rev_args, ai_rules = rules }) cont
  = do {
	; let args = reverse rev_args
              env' = zapSubstEnv env
	; mb_rule <- tryRules env rules fun args cont
	; case mb_rule of {
	     Just (n_args, rule_rhs) -> simplExprF env' rule_rhs $
                                        pushSimplifiedArgs env' (drop n_args args) cont ;
           ; Nothing -> rebuild env (mkApps (Var fun) args) cont
    } }
tryRules :: SimplEnv -> [CoreRule]
         -> Id -> [OutExpr] -> SimplCont 
	 -> SimplM (Maybe (Arity, CoreExpr))
tryRules env rules fn args call_cont
  | null rules
  = return Nothing
  | otherwise
  = do { dflags <- getDOptsSmpl
       ; case activeRule dflags env of {
           Nothing     -> return Nothing  ;
           Just act_fn -> 
         case lookupRule act_fn (getUnfoldingInRuleMatch env) (getInScope env) fn args rules of {
           Nothing               -> return Nothing ;
           Just (rule, rule_rhs) ->

             do { tick (RuleFired (ru_name rule))
                ; trace_dump dflags rule rule_rhs $
                  return (Just (ruleArity rule, rule_rhs)) }}}}
  where
    trace_dump dflags rule rule_rhs stuff
      | not (dopt Opt_D_dump_rule_firings dflags)
      , not (dopt Opt_D_dump_rule_rewrites dflags) = stuff
      | not (dopt Opt_D_dump_rule_rewrites dflags)

      = pprTrace "Rule fired:" (ftext (ru_name rule)) stuff
      | otherwise
      = pprTrace "Rule fired"
           (vcat [text "Rule:" <+> ftext (ru_name rule),
           	  text "Before:" <+> hang (ppr fn) 2 (sep (map pprParendExpr args)),
           	  text "After: " <+> pprCoreExpr rule_rhs,
           	  text "Cont:  " <+> ppr call_cont])
           stuff

rebuildCase, reallyRebuildCase
   :: SimplEnv
   -> OutExpr
   -> InId
   -> [InAlt]
   -> SimplCont
   -> SimplM (SimplEnv, OutExpr)

rebuildCase env scrut case_bndr alts cont
  | Lit lit <- scrut
  = do  { tick (KnownBranch case_bndr)
        ; case findAlt (LitAlt lit) alts of
	    Nothing           -> missingAlt env case_bndr alts cont
	    Just (_, bs, rhs) -> simple_rhs bs rhs }

  | Just (con, ty_args, other_args) <- exprIsConApp_maybe (getUnfoldingInRuleMatch env) scrut
  = do  { tick (KnownBranch case_bndr)
        ; case findAlt (DataAlt con) alts of
	    Nothing  -> missingAlt env case_bndr alts cont
            Just (DEFAULT, bs, rhs) -> simple_rhs bs rhs
	    Just (_, bs, rhs)       -> knownCon env scrut con ty_args other_args 
                                                case_bndr bs rhs cont
	}
  where
    simple_rhs bs rhs = ASSERT( null bs ) 
                        do { env' <- simplNonRecX env case_bndr scrut
    	                   ; simplExprF env' rhs cont }

rebuildCase env scrut case_bndr [(_, bndrs, rhs)] cont
 | all isDeadBinder bndrs

 , if isUnLiftedType (idType case_bndr)
   then exprOkForSpeculation scrut

   else exprIsHNF scrut || var_demanded_later scrut
  = do  { tick (CaseElim case_bndr)
        ; env' <- simplNonRecX env case_bndr scrut
        ; simplExprF env' rhs cont }
  where
    var_demanded_later (Var v) = isStrictDmd (idDemandInfo case_bndr)
                                 && not (isTickBoxOp v)
    var_demanded_later _       = False

rebuildCase env scrut case_bndr alts@[(_, bndrs, rhs)] cont
  | all isDeadBinder (case_bndr : bndrs)
  = do { let rhs' = substExpr (text "rebuild-case") env rhs
             out_args = [Type (substTy env (idType case_bndr)), 
	     	         Type (exprType rhs'), scrut, rhs']

       ; rule_base <- getSimplRules
       ; mb_rule <- tryRules env (getRules rule_base seqId) seqId out_args cont
       ; case mb_rule of 
           Just (n_args, res) -> simplExprF (zapSubstEnv env) 
	   	       		    	    (mkApps res (drop n_args out_args))
                                            cont
	   Nothing -> reallyRebuildCase env scrut case_bndr alts cont }

rebuildCase env scrut case_bndr alts cont
  = reallyRebuildCase env scrut case_bndr alts cont

reallyRebuildCase env scrut case_bndr alts cont
  = do  {
          (env', dup_cont, nodup_cont) <- prepareCaseCont env alts cont

        ; (scrut', case_bndr', alts') <- simplAlts env' scrut case_bndr alts dup_cont

	; if null alts' then missingAlt env case_bndr alts cont
	  else do
        { dflags <- getDOptsSmpl
        ; case_expr <- mkCase dflags scrut' case_bndr' alts'

	; rebuild env' case_expr nodup_cont } }
simplAlts :: SimplEnv
          -> OutExpr
          -> InId
          -> [InAlt]
	  -> SimplCont
          -> SimplM (OutExpr, OutId, [OutAlt])

simplAlts env scrut case_bndr alts cont'
  =
    do  { let env0 = zapFloats env

        ; (env1, case_bndr1) <- simplBinder env0 case_bndr

        ; fam_envs <- getFamEnvs
	; (alt_env', scrut', case_bndr') <- improveSeq fam_envs env1 scrut 
						       case_bndr case_bndr1 alts

        ; (imposs_deflt_cons, in_alts) <- prepareAlts scrut' case_bndr' alts

        ; alts' <- mapM (simplAlt alt_env' imposs_deflt_cons case_bndr' cont') in_alts
        ; return (scrut', case_bndr', alts') }

improveSeq :: (FamInstEnv, FamInstEnv) -> SimplEnv
	   -> OutExpr -> InId -> OutId -> [InAlt]
	   -> SimplM (SimplEnv, OutExpr, OutId)
improveSeq fam_envs env scrut case_bndr case_bndr1 [(DEFAULT,_,_)]
  | not (isDeadBinder case_bndr)
  , Just (co, ty2) <- topNormaliseType fam_envs (idType case_bndr1)
  = do { case_bndr2 <- newId (fsLit "nt") ty2
        ; let rhs  = DoneEx (Var case_bndr2 `Cast` mkSymCoercion co)
              env2 = extendIdSubst env case_bndr rhs
        ; return (env2, scrut `Cast` co, case_bndr2) }

improveSeq _ env scrut _ case_bndr1 _
  = return (env, scrut, case_bndr1)

simplAlt :: SimplEnv
         -> [AltCon]
         -> OutId
         -> SimplCont
         -> InAlt
         -> SimplM OutAlt

simplAlt env imposs_deflt_cons case_bndr' cont' (DEFAULT, bndrs, rhs)
  = ASSERT( null bndrs )
    do  { let env' = addBinderOtherCon env case_bndr' imposs_deflt_cons
        ; rhs' <- simplExprC env' rhs cont'
        ; return (DEFAULT, [], rhs') }

simplAlt env _ case_bndr' cont' (LitAlt lit, bndrs, rhs)
  = ASSERT( null bndrs )
    do  { let env' = addBinderUnfolding env case_bndr' (Lit lit)
        ; rhs' <- simplExprC env' rhs cont'
        ; return (LitAlt lit, [], rhs') }

simplAlt env _ case_bndr' cont' (DataAlt con, vs, rhs)
  = do  {
          let vs_with_evals = add_evals (dataConRepStrictness con)
        ; (env', vs') <- simplLamBndrs env vs_with_evals

        ; let inst_tys' = tyConAppArgs (idType case_bndr')
              con_args  = map Type inst_tys' ++ varsToCoreExprs vs'
              env''     = addBinderUnfolding env' case_bndr'
                                             (mkConApp con con_args)

        ; rhs' <- simplExprC env'' rhs cont'
        ; return (DataAlt con, vs', rhs') }
  where
    add_evals the_strs
        = go vs the_strs
        where
          go [] [] = []
          go (v:vs') strs | isTyCoVar v = v : go vs' strs
          go (v:vs') (str:strs)
            | isMarkedStrict str = evald_v  : go vs' strs
            | otherwise          = zapped_v : go vs' strs
            where
              zapped_v = zap_occ_info v
              evald_v  = zapped_v `setIdUnfolding` evaldUnfolding
          go _ _ = pprPanic "cat_evals" (ppr con $$ ppr vs $$ ppr the_strs)

    zap_occ_info = zapCasePatIdOcc case_bndr'

addBinderUnfolding :: SimplEnv -> Id -> CoreExpr -> SimplEnv
addBinderUnfolding env bndr rhs
  = modifyInScope env (bndr `setIdUnfolding` mkSimpleUnfolding rhs)

addBinderOtherCon :: SimplEnv -> Id -> [AltCon] -> SimplEnv
addBinderOtherCon env bndr cons
  = modifyInScope env (bndr `setIdUnfolding` mkOtherCon cons)

zapCasePatIdOcc :: Id -> Id -> Id
zapCasePatIdOcc case_bndr
  | isDeadBinder case_bndr = \ pat_id -> pat_id
  | otherwise	 	   = \ pat_id -> zapIdOccInfo pat_id
knownCon :: SimplEnv		
         -> OutExpr
         -> DataCon -> [OutType] -> [OutExpr]
         -> InId -> [InBndr] -> InExpr
         -> SimplCont
         -> SimplM (SimplEnv, OutExpr)

knownCon env scrut dc dc_ty_args dc_args bndr bs rhs cont
  = do  { env'  <- bind_args env bs dc_args
        ; env'' <- bind_case_bndr env'
        ; simplExprF env'' rhs cont }
  where
    zap_occ = zapCasePatIdOcc bndr

    bind_args env' [] _  = return env'

    bind_args env' (b:bs') (Type ty : args)
      = ASSERT( isTyCoVar b )
        bind_args (extendTvSubst env' b ty) bs' args

    bind_args env' (b:bs') (arg : args)
      = ASSERT( isId b )
        do { let b' = zap_occ b
           ; env'' <- simplNonRecX env' b' arg
           ; bind_args env'' bs' args }

    bind_args _ _ _ =
      pprPanic "bind_args" $ ppr dc $$ ppr bs $$ ppr dc_args $$
                             text "scrut:" <+> ppr scrut

    bind_case_bndr env
      | isDeadBinder bndr   = return env
      | exprIsTrivial scrut = return (extendIdSubst env bndr (DoneEx scrut))
      | otherwise           = do { dc_args <- mapM (simplVar env) bs
		    	         ; let con_app = Var (dataConWorkId dc) 
                          			 `mkTyApps` dc_ty_args      
                          			 `mkApps`   dc_args
      			         ; simplNonRecX env bndr con_app }
  
missingAlt :: SimplEnv -> Id -> [InAlt] -> SimplCont -> SimplM (SimplEnv, OutExpr)
missingAlt env case_bndr alts cont
  = WARN( True, ptext (sLit "missingAlt") <+> ppr case_bndr )
    return (env, mkImpossibleExpr res_ty)
  where
    res_ty = contResultType env (substTy env (coreAltsType alts)) cont
prepareCaseCont :: SimplEnv
                -> [InAlt] -> SimplCont
                -> SimplM (SimplEnv, SimplCont,SimplCont)

prepareCaseCont env [_] cont = return (env, cont, mkBoringStop)
prepareCaseCont env _   cont = mkDupableCont env cont
mkDupableCont :: SimplEnv -> SimplCont
              -> SimplM (SimplEnv, SimplCont, SimplCont)

mkDupableCont env cont
  | contIsDupable cont
  = return (env, cont, mkBoringStop)

mkDupableCont _   (Stop {}) = panic "mkDupableCont"

mkDupableCont env (CoerceIt ty cont)
  = do  { (env', dup, nodup) <- mkDupableCont env cont
        ; return (env', CoerceIt ty dup, nodup) }

mkDupableCont env cont@(StrictBind {})
  =  return (env, mkBoringStop, cont)

mkDupableCont env (StrictArg info cci cont)
  = do { (env', dup, nodup) <- mkDupableCont env cont
       ; (env'', args')     <- mapAccumLM (makeTrivial NotTopLevel) env' (ai_args info)
       ; return (env'', StrictArg (info { ai_args = args' }) cci dup, nodup) }

mkDupableCont env (ApplyTo _ arg se cont)
  =
    do  { (env', dup_cont, nodup_cont) <- mkDupableCont env cont
        ; arg' <- simplExpr (se `setInScope` env') arg
        ; (env'', arg'') <- makeTrivial NotTopLevel env' arg'
        ; let app_cont = ApplyTo OkToDup arg'' (zapSubstEnv env'') dup_cont
        ; return (env'', app_cont, nodup_cont) }

mkDupableCont env cont@(Select _ case_bndr [(_, bs, _rhs)] _ _)
  | all isDeadBinder bs
    && not (isUnLiftedType (idType case_bndr))
  = return (env, mkBoringStop, cont)

mkDupableCont env (Select _ case_bndr alts se cont)
  =
    do  { tick (CaseOfCase case_bndr)
        ; (env', dup_cont, nodup_cont) <- mkDupableCont env cont

        ; let alt_env = se `setInScope` env'
        ; (alt_env', case_bndr') <- simplBinder alt_env case_bndr
        ; alts' <- mapM (simplAlt alt_env' [] case_bndr' dup_cont) alts

        ; (env'', alts'') <- mkDupableAlts env' case_bndr' alts'
        ; return (env'',
                  Select OkToDup case_bndr' alts'' (zapSubstEnv env'') mkBoringStop,
                  nodup_cont) }

mkDupableAlts :: SimplEnv -> OutId -> [InAlt]
              -> SimplM (SimplEnv, [InAlt])

mkDupableAlts env case_bndr' the_alts
  = go env the_alts
  where
    go env0 [] = return (env0, [])
    go env0 (alt:alts)
        = do { (env1, alt') <- mkDupableAlt env0 case_bndr' alt
             ; (env2, alts') <- go env1 alts
             ; return (env2, alt' : alts' ) }

mkDupableAlt :: SimplEnv -> OutId -> (AltCon, [CoreBndr], CoreExpr)
              -> SimplM (SimplEnv, (AltCon, [CoreBndr], CoreExpr))
mkDupableAlt env case_bndr (con, bndrs', rhs')
  | exprIsDupable rhs'
  = return (env, (con, bndrs', rhs'))
  | otherwise
  = do  { let rhs_ty'  = exprType rhs'
    	      scrut_ty = idType case_bndr
    	      case_bndr_w_unf	
                = case con of 
		      DEFAULT    -> case_bndr					
	      	      DataAlt dc -> setIdUnfolding case_bndr unf
		      	  where
		      	     unf = mkInlineUnfolding Nothing rhs
		      	     rhs = mkConApp dc (map Type (tyConAppArgs scrut_ty)
			     	   	        ++ varsToCoreExprs bndrs')

		      LitAlt {} -> WARN( True, ptext (sLit "mkDupableAlt")
		      	     	   	        <+> ppr case_bndr <+> ppr con )
			           case_bndr

              used_bndrs' | isDeadBinder case_bndr = filter abstract_over bndrs'
			  | otherwise		   = bndrs' ++ [case_bndr_w_unf]
	      
              abstract_over bndr
                  | isTyCoVar bndr = True
                  | otherwise    = not (isDeadBinder bndr)

        ; (final_bndrs', final_args)
                <- if (any isId used_bndrs')
                   then return (used_bndrs', varsToCoreExprs used_bndrs')
                    else do { rw_id <- newId (fsLit "w") realWorldStatePrimTy
                            ; return ([rw_id], [Var realWorldPrimId]) }

        ; join_bndr <- newId (fsLit "$j") (mkPiTypes final_bndrs' rhs_ty')

        ; let
                really_final_bndrs     = map one_shot final_bndrs'
                one_shot v | isId v    = setOneShotLambda v
                           | otherwise = v
                join_rhs  = mkLams really_final_bndrs rhs'
                join_call = mkApps (Var join_bndr) final_args

	; env' <- addPolyBind NotTopLevel env (NonRec join_bndr join_rhs)
        ; return (env', (con, bndrs', join_call)) }
