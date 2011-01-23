module ByteCodeGen ( UnlinkedBCO, byteCodeGen, coreExprToBCOs ) where

#include "HsVersions.h"

import ByteCodeInstr
import ByteCodeItbls
import ByteCodeAsm
import ByteCodeLink
import LibFFI

import Outputable
import Name
import MkId
import Id
import ForeignCall
import HscTypes
import CoreUtils
import CoreSyn
import PprCore
import Literal
import PrimOp
import CoreFVs
import Type
import DataCon
import TyCon
import Util
import Var
import VarSet
import TysPrim
import DynFlags
import ErrUtils
import Unique
import FastString
import Panic
import SMRep
import Bitmap
import OrdList
import Constants

import Data.List
import Foreign
import Foreign.C

import Control.Monad	( when )
import Data.Char

import UniqSupply
import BreakArray
import Data.Maybe
import Module 
import IdInfo 

import Data.Map (Map)
import qualified Data.Map as Map
import qualified FiniteMap as Map

byteCodeGen :: DynFlags
            -> [CoreBind]
	    -> [TyCon]
            -> ModBreaks 
            -> IO CompiledByteCode
byteCodeGen dflags binds tycs modBreaks 
   = do showPass dflags "ByteCodeGen"

        let flatBinds = [ (bndr, freeVars rhs) 
			| (bndr, rhs) <- flattenBinds binds]

        us <- mkSplitUniqSupply 'y'  
        (BcM_State _us _final_ctr mallocd _, proto_bcos) 
           <- runBc us modBreaks (mapM schemeTopBind flatBinds)  

        when (notNull mallocd)
             (panic "ByteCodeGen.byteCodeGen: missing final emitBc?")

        dumpIfSet_dyn dflags Opt_D_dump_BCOs
           "Proto-BCOs" (vcat (intersperse (char ' ') (map ppr proto_bcos)))

        assembleBCOs proto_bcos tycs
        
coreExprToBCOs :: DynFlags
	       -> CoreExpr
               -> IO UnlinkedBCO
coreExprToBCOs dflags expr
 = do showPass dflags "ByteCodeGen"

      let invented_name  = mkSystemVarName (mkPseudoUniqueE 0) (fsLit "ExprTopLevel")
          invented_id    = Id.mkLocalId invented_name (panic "invented_id's type")
	  
      us <- mkSplitUniqSupply 'y'
      (BcM_State _us _final_ctr mallocd _ , proto_bco)  
         <- runBc us emptyModBreaks (schemeTopBind (invented_id, freeVars expr))

      when (notNull mallocd)
           (panic "ByteCodeGen.coreExprToBCOs: missing final emitBc?")

      dumpIfSet_dyn dflags Opt_D_dump_BCOs "Proto-BCOs" (ppr proto_bco)

      assembleBCO proto_bco

type BCInstrList = OrdList BCInstr

type Sequel = Word16

type BCEnv = Map Id Word16

mkProtoBCO
   :: name
   -> BCInstrList
   -> Either  [AnnAlt Id VarSet] (AnnExpr Id VarSet)
   -> Int
   -> Word16
   -> [StgWord]
   -> Bool
   -> [BcPtr]
   -> ProtoBCO name
mkProtoBCO nm instrs_ordlist origin arity bitmap_size bitmap is_ret mallocd_blocks 
   = ProtoBCO {
	protoBCOName = nm,
	protoBCOInstrs = maybe_with_stack_check,
	protoBCOBitmap = bitmap,
	protoBCOBitmapSize = bitmap_size,
	protoBCOArity = arity,
	protoBCOExpr = origin,
	protoBCOPtrs = mallocd_blocks
      }
     where
        maybe_with_stack_check
	   | is_ret && stack_usage < fromIntegral aP_STACK_SPLIM = peep_d
           | stack_usage >= fromIntegral iNTERP_STACK_CHECK_THRESH
           = STKCHECK stack_usage : peep_d
           | otherwise
           = peep_d
             
        stack_usage = sum (map bciStackUse peep_d)

        peep_d = peep (fromOL instrs_ordlist)

        peep (PUSH_L off1 : PUSH_L off2 : PUSH_L off3 : rest)
           = PUSH_LLL off1 (off2-1) (off3-2) : peep rest
        peep (PUSH_L off1 : PUSH_L off2 : rest)
           = PUSH_LL off1 (off2-1) : peep rest
        peep (i:rest)
           = i : peep rest
        peep []
           = []

argBits :: [CgRep] -> [Bool]
argBits [] = []
argBits (rep : args)
  | isFollowableArg rep = False : argBits args
  | otherwise = take (cgRepSizeW rep) (repeat True) ++ argBits args

schemeTopBind :: (Id, AnnExpr Id VarSet) -> BcM (ProtoBCO Name)

schemeTopBind (id, rhs) 
  | Just data_con <- isDataConWorkId_maybe id,
    isNullaryRepDataCon data_con = do
    emitBc (mkProtoBCO (getName id) (toOL [PACK data_con 0, ENTER])
                       (Right rhs) 0 0 [] False) 

  | otherwise
  = schemeR [] (id, rhs)

schemeR :: [Id]
	-> (Id, AnnExpr Id VarSet)
	-> BcM (ProtoBCO Name)
schemeR fvs (nm, rhs)

   = schemeR_wrk fvs nm rhs (collect rhs)

collect :: AnnExpr Id VarSet -> ([Var], AnnExpr' Id VarSet)
collect (_, e) = go [] e
  where
    go xs e | Just e' <- bcView e = go xs e'
    go xs (AnnLam x (_,e))        = go (x:xs) e
    go xs not_lambda              = (reverse xs, not_lambda)

schemeR_wrk :: [Id] -> Id -> AnnExpr Id VarSet -> ([Var], AnnExpr' Var VarSet) -> BcM (ProtoBCO Name) 
schemeR_wrk fvs nm original_body (args, body)
   = let 
	 all_args  = reverse args ++ fvs
	 arity     = length all_args

         szsw_args = map (fromIntegral . idSizeW) all_args
         szw_args  = sum szsw_args
         p_init    = Map.fromList (zip all_args (mkStackOffsets 0 szsw_args))

	 bits = argBits (reverse (map idCgRep all_args))
	 bitmap_size = genericLength bits
	 bitmap = mkBitmap bits
     in do
     body_code <- schemeER_wrk szw_args p_init body   
 
     emitBc (mkProtoBCO (getName nm) body_code (Right original_body)
		arity bitmap_size bitmap False)

schemeER_wrk :: Word16 -> BCEnv -> AnnExpr' Id VarSet -> BcM BCInstrList
schemeER_wrk d p rhs
   | Just (tickInfo, (_annot, newRhs)) <- isTickedExp' rhs = do 
        code <- schemeE d 0 p newRhs 
        arr <- getBreakArray 
        let idOffSets = getVarOffSets d p tickInfo
        let tickNumber = tickInfo_number tickInfo
        let breakInfo = BreakInfo 
                        { breakInfo_module = tickInfo_module tickInfo
                        , breakInfo_number = tickNumber 
                        , breakInfo_vars = idOffSets
                        , breakInfo_resty = exprType (deAnnotate' newRhs)
                        }
        let breakInstr = case arr of
                         BA arr# ->
                             BRK_FUN arr# (fromIntegral tickNumber) breakInfo
        return $ breakInstr `consOL` code
   | otherwise = schemeE d 0 p rhs 

getVarOffSets :: Word16 -> BCEnv -> TickInfo -> [(Id, Word16)]
getVarOffSets d p = catMaybes . map (getOffSet d p) . tickInfo_locals 

getOffSet :: Word16 -> BCEnv -> Id -> Maybe (Id, Word16)
getOffSet d env id 
   = case lookupBCEnv_maybe id env of
        Nothing     -> Nothing 
        Just offset -> Just (id, d - offset)

fvsToEnv :: BCEnv -> VarSet -> [Id]
fvsToEnv p fvs = [v | v <- varSetElems fvs, 
		      isId v,
		      v `Map.member` p]

data TickInfo 
   = TickInfo   
     { tickInfo_number :: Int
     , tickInfo_module :: Module
     , tickInfo_locals :: [Id]
     } 

instance Outputable TickInfo where
   ppr info = text "TickInfo" <+> 
              parens (int (tickInfo_number info) <+> ppr (tickInfo_module info) <+>
                      ppr (tickInfo_locals info))

schemeE :: Word16 -> Sequel -> BCEnv -> AnnExpr' Id VarSet -> BcM BCInstrList

schemeE d s p e
   | Just e' <- bcView e
   = schemeE d s p e'

schemeE d s p e@(AnnApp _ _) 
   = schemeT d s p e

schemeE d s p e@(AnnVar v)
   | not (isUnLiftedType v_type)
   =
     schemeT d s p e

   | otherwise
   = do
        (push, szw) <- pushAtom d p (AnnVar v)
        return (push
                  `appOL`  mkSLIDE szw (d-s)
                  `snocOL` RETURN_UBX v_rep)
   where
      v_type = idType v
      v_rep = typeCgRep v_type

schemeE d s p (AnnLit literal)
   = do (push, szw) <- pushAtom d p (AnnLit literal)
        let l_rep = typeCgRep (literalType literal)
        return (push
               `appOL`  mkSLIDE szw (d-s)
               `snocOL` RETURN_UBX l_rep)

schemeE d s p (AnnLet (AnnNonRec x (_,rhs)) (_,body))
   | (AnnVar v, args_r_to_l) <- splitApp rhs,
     Just data_con <- isDataConWorkId_maybe v,
     dataConRepArity data_con == length args_r_to_l
   = do
        alloc_code <- mkConAppCode d s p data_con args_r_to_l
        body_code <- schemeE (d+1) s (Map.insert x d p) body
        return (alloc_code `appOL` body_code)

schemeE d s p (AnnLet binds (_,body))
   = let (xs,rhss) = case binds of AnnNonRec x rhs  -> ([x],[rhs])
                                   AnnRec xs_n_rhss -> unzip xs_n_rhss
         n_binds = genericLength xs

         fvss  = map (fvsToEnv p' . fst) rhss

         sizes = map (\rhs_fvs -> sum (map (fromIntegral . idSizeW) rhs_fvs)) fvss

	 arities = map (genericLength . fst . collect) rhss

         p'    = Map.insertList (zipE xs (mkStackOffsets d (genericReplicate n_binds 1))) p
         d'    = d + n_binds
         zipE  = zipEqual "schemeE"

         build_thunk _ [] size bco off arity
            = return (PUSH_BCO bco `consOL` unitOL (mkap (off+size) size))
	   where 
		mkap | arity == 0 = MKAP
		     | otherwise  = MKPAP
         build_thunk dd (fv:fvs) size bco off arity = do
              (push_code, pushed_szw) <- pushAtom dd p' (AnnVar fv) 
              more_push_code <- build_thunk (dd+pushed_szw) fvs size bco off arity
              return (push_code `appOL` more_push_code)

         alloc_code = toOL (zipWith mkAlloc sizes arities)
	   where mkAlloc sz 0
                    | is_tick     = ALLOC_AP_NOUPD sz
                    | otherwise   = ALLOC_AP sz
		 mkAlloc sz arity = ALLOC_PAP arity sz

         is_tick = case binds of 
                     AnnNonRec id _ -> occNameFS (getOccName id) == tickFS
                     _other -> False

	 compile_bind d' fvs x rhs size arity off = do
		bco <- schemeR fvs (x,rhs)
		build_thunk d' fvs size bco off arity

	 compile_binds = 
	    [ compile_bind d' fvs x rhs size arity n
	    | (fvs, x, rhs, size, arity, n) <- 
		zip6 fvss xs rhss sizes arities [n_binds, n_binds-1 .. 1]
	    ]
     in do
     body_code <- schemeE d' s p' body
     thunk_codes <- sequence compile_binds
     return (alloc_code `appOL` concatOL thunk_codes `appOL` body_code)

schemeE d s p exp@(AnnCase {})
   | Just (_tickInfo, _rhs) <- isTickedExp' exp
   = if isUnLiftedType ty
        then do
          id <- newId (mkFunTy realWorldStatePrimTy ty)
          st <- newId realWorldStatePrimTy
          let letExp = AnnLet (AnnNonRec id (fvs, AnnLam st (emptyVarSet, exp)))
                              (emptyVarSet, (AnnApp (emptyVarSet, AnnVar id) 
                                                    (emptyVarSet, AnnVar realWorldPrimId)))
          schemeE d s p letExp
        else do
          id <- newId ty
          let letExp = AnnLet (AnnNonRec id (fvs, exp)) (emptyVarSet, AnnVar id)
          schemeE d s p letExp
   where exp' = deAnnotate' exp
         fvs  = exprFreeVars exp'
         ty   = exprType exp'

schemeE d s p (AnnCase scrut _ _ [(DataAlt dc, [bind1, bind2], rhs)])
   | isUnboxedTupleCon dc, VoidArg <- typeCgRep (idType bind1)

   =
     doCase d s p scrut bind2 [(DEFAULT, [], rhs)] True 

   | isUnboxedTupleCon dc, VoidArg <- typeCgRep (idType bind2)
   =
     doCase d s p scrut bind1 [(DEFAULT, [], rhs)] True 

schemeE d s p (AnnCase scrut _ _ [(DataAlt dc, [bind1], rhs)])
   | isUnboxedTupleCon dc
   =
     doCase d s p scrut bind1 [(DEFAULT, [], rhs)] True 

schemeE d s p (AnnCase scrut bndr _ alts)
   = doCase d s p scrut bndr alts False 

schemeE _ _ _ expr
   = pprPanic "ByteCodeGen.schemeE: unhandled case" 
               (pprCoreExpr (deAnnotate' expr))

isTickedExp' :: AnnExpr' Id a -> Maybe (TickInfo, AnnExpr Id a)
isTickedExp' (AnnCase scrut _bndr _type alts)
   | Just tickInfo <- isTickedScrut scrut,
     [(DEFAULT, _bndr, rhs)] <- alts 
     = Just (tickInfo, rhs)
   where
   isTickedScrut :: (AnnExpr Id a) -> Maybe TickInfo 
   isTickedScrut expr
      | Var id <- f,
        Just (TickBox modName tickNumber) <- isTickBoxOp_maybe id
           = Just $ TickInfo { tickInfo_number = tickNumber
                             , tickInfo_module = modName
                             , tickInfo_locals = idsOfArgs args
                             }
      | otherwise = Nothing
      where
      (f, args) = collectArgs $ deAnnotate expr
      idsOfArgs :: [Expr Id] -> [Id]
      idsOfArgs = catMaybes . map exprId 
      exprId :: Expr Id -> Maybe Id
      exprId (Var id) = Just id
      exprId _        = Nothing

isTickedExp' _ = Nothing

schemeT :: Word16
        -> Sequel
        -> BCEnv
        -> AnnExpr' Id VarSet 
        -> BcM BCInstrList

schemeT d s p app

   | Just (arg, constr_names) <- maybe_is_tagToEnum_call
   = do (push, arg_words) <- pushAtom d p arg
        tagToId_sequence <- implement_tagToId constr_names
        return (push `appOL`  tagToId_sequence            
                       `appOL`  mkSLIDE 1 (d+arg_words-s)
                       `snocOL` ENTER)

   | Just (CCall ccall_spec) <- isFCallId_maybe fn
   = generateCCall d s p ccall_spec fn args_r_to_l

   | Just con <- maybe_saturated_dcon,
     isUnboxedTupleCon con
   = case args_r_to_l of
	[arg1,arg2] | isVoidArgAtom arg1 -> 
		  unboxedTupleReturn d s p arg2
	[arg1,arg2] | isVoidArgAtom arg2 -> 
		  unboxedTupleReturn d s p arg1
	_other -> unboxedTupleException

   | Just con <- maybe_saturated_dcon
   = do alloc_con <- mkConAppCode d s p con args_r_to_l
        return (alloc_con	 `appOL` 
                  mkSLIDE 1 (d - s) `snocOL`
                  ENTER)

   | otherwise
   = doTailCall d s p fn args_r_to_l

   where
      maybe_is_tagToEnum_call
         = let extract_constr_Names ty
                 | Just (tyc, _) <- splitTyConApp_maybe (repType ty),
		   isDataTyCon tyc
		   = map (getName . dataConWorkId) (tyConDataCons tyc)
		 | otherwise
                   = pprPanic "maybe_is_tagToEnum_call.extract_constr_Ids" (ppr ty)
           in
           case app of
              (AnnApp (_, AnnApp (_, AnnVar v) (_, AnnType t)) arg)
                 -> case isPrimOpId_maybe v of
                       Just TagToEnumOp -> Just (snd arg, extract_constr_Names t)
		       _		-> Nothing
              _ -> Nothing

      (AnnVar fn, args_r_to_l) = splitApp app

      n_args = length args_r_to_l
      maybe_saturated_dcon  
	= case isDataConWorkId_maybe fn of
		Just con | dataConRepArity con == n_args -> Just con
		_ -> Nothing

mkConAppCode :: Word16 -> Sequel -> BCEnv
	     -> DataCon
	     -> [AnnExpr' Id VarSet]
	     -> BcM BCInstrList

mkConAppCode _ _ _ con []
  = ASSERT( isNullaryRepDataCon con )
    return (unitOL (PUSH_G (getName (dataConWorkId con))))

mkConAppCode orig_d _ p con args_r_to_l 
  = ASSERT( dataConRepArity con == length args_r_to_l )
    do_pushery orig_d (non_ptr_args ++ ptr_args)
 where
      (ptr_args, non_ptr_args) = partition isPtrAtom args_r_to_l

      do_pushery d (arg:args)
         = do (push, arg_words) <- pushAtom d p arg
              more_push_code <- do_pushery (d+arg_words) args
              return (push `appOL` more_push_code)
      do_pushery d []
         = return (unitOL (PACK con n_arg_words))
	 where
	   n_arg_words = d - orig_d

unboxedTupleReturn
	:: Word16 -> Sequel -> BCEnv
	-> AnnExpr' Id VarSet -> BcM BCInstrList
unboxedTupleReturn d s p arg = do
  (push, sz) <- pushAtom d p arg
  return (push `appOL`
	    mkSLIDE sz (d-s) `snocOL`
	    RETURN_UBX (atomRep arg))

doTailCall
	:: Word16 -> Sequel -> BCEnv
	-> Id -> [AnnExpr' Id VarSet]
	-> BcM BCInstrList
doTailCall init_d s p fn args
  = do_pushes init_d args (map atomRep args)
  where
  do_pushes d [] reps = do
	ASSERT( null reps ) return ()
        (push_fn, sz) <- pushAtom d p (AnnVar fn)
	ASSERT( sz == 1 ) return ()
	return (push_fn `appOL` (
		  mkSLIDE ((d-init_d) + 1) (init_d - s) `appOL`
		  unitOL ENTER))
  do_pushes d args reps = do
      let (push_apply, n, rest_of_reps) = findPushSeq reps
	  (these_args, rest_of_args) = splitAt n args
      (next_d, push_code) <- push_seq d these_args
      instrs <- do_pushes (next_d + 1) rest_of_args rest_of_reps 
      return (push_code `appOL` (push_apply `consOL` instrs))

  push_seq d [] = return (d, nilOL)
  push_seq d (arg:args) = do
    (push_code, sz) <- pushAtom d p arg 
    (final_d, more_push_code) <- push_seq (d+sz) args
    return (final_d, push_code `appOL` more_push_code)

findPushSeq :: [CgRep] -> (BCInstr, Int, [CgRep])
findPushSeq (PtrArg: PtrArg: PtrArg: PtrArg: PtrArg: PtrArg: rest)
  = (PUSH_APPLY_PPPPPP, 6, rest)
findPushSeq (PtrArg: PtrArg: PtrArg: PtrArg: PtrArg: rest)
  = (PUSH_APPLY_PPPPP, 5, rest)
findPushSeq (PtrArg: PtrArg: PtrArg: PtrArg: rest)
  = (PUSH_APPLY_PPPP, 4, rest)
findPushSeq (PtrArg: PtrArg: PtrArg: rest)
  = (PUSH_APPLY_PPP, 3, rest)
findPushSeq (PtrArg: PtrArg: rest)
  = (PUSH_APPLY_PP, 2, rest)
findPushSeq (PtrArg: rest)
  = (PUSH_APPLY_P, 1, rest)
findPushSeq (VoidArg: rest)
  = (PUSH_APPLY_V, 1, rest)
findPushSeq (NonPtrArg: rest)
  = (PUSH_APPLY_N, 1, rest)
findPushSeq (FloatArg: rest)
  = (PUSH_APPLY_F, 1, rest)
findPushSeq (DoubleArg: rest)
  = (PUSH_APPLY_D, 1, rest)
findPushSeq (LongArg: rest)
  = (PUSH_APPLY_L, 1, rest)
findPushSeq _
  = panic "ByteCodeGen.findPushSeq"

doCase  :: Word16 -> Sequel -> BCEnv
	-> AnnExpr Id VarSet -> Id -> [AnnAlt Id VarSet]
	-> Bool
	-> BcM BCInstrList
doCase d s p (_,scrut) bndr alts is_unboxed_tuple 
  = let
        ret_frame_sizeW = 2

	unlifted_itbl_sizeW | isAlgCase = 0
	  		    | otherwise = 1

	d_bndr = d + ret_frame_sizeW + fromIntegral (idSizeW bndr)

        d_alts = d_bndr + unlifted_itbl_sizeW

        p_alts = Map.insert bndr (d_bndr - 1) p

	bndr_ty = idType bndr
        isAlgCase = not (isUnLiftedType bndr_ty) && not is_unboxed_tuple

	codeAlt (DEFAULT, _, (_,rhs))
	   = do rhs_code <- schemeE d_alts s p_alts rhs
	        return (NoDiscr, rhs_code)

        codeAlt alt@(_, bndrs, (_,rhs))
	   | null real_bndrs = do
		rhs_code <- schemeE d_alts s p_alts rhs
                return (my_discr alt, rhs_code)
           | otherwise =
             let
		 (ptrs,nptrs) = partition (isFollowableArg.idCgRep) real_bndrs
		 ptr_sizes    = map (fromIntegral . idSizeW) ptrs
		 nptrs_sizes  = map (fromIntegral . idSizeW) nptrs
		 bind_sizes   = ptr_sizes ++ nptrs_sizes
		 size         = sum ptr_sizes + sum nptrs_sizes
		 p' = Map.insertList
			(zip (reverse (ptrs ++ nptrs))
			  (mkStackOffsets d_alts (reverse bind_sizes)))
                        p_alts 
	     in do
             MASSERT(isAlgCase)
	     rhs_code <- schemeE (d_alts+size) s p' rhs
             return (my_discr alt, unitOL (UNPACK size) `appOL` rhs_code)
	   where
	     real_bndrs = filter (not.isTyCoVar) bndrs

        my_discr (DEFAULT, _, _) = NoDiscr 
        my_discr (DataAlt dc, _, _) 
           | isUnboxedTupleCon dc
           = unboxedTupleException
           | otherwise
           = DiscrP (fromIntegral (dataConTag dc - fIRST_TAG))
        my_discr (LitAlt l, _, _)
           = case l of MachInt i     -> DiscrI (fromInteger i)
                       MachWord w    -> DiscrW (fromInteger w)
                       MachFloat r   -> DiscrF (fromRational r)
                       MachDouble r  -> DiscrD (fromRational r)
                       MachChar i    -> DiscrI (ord i)
                       _ -> pprPanic "schemeE(AnnCase).my_discr" (ppr l)

        maybe_ncons 
           | not isAlgCase = Nothing
           | otherwise 
           = case [dc | (DataAlt dc, _, _) <- alts] of
                []     -> Nothing
                (dc:_) -> Just (tyConFamilySize (dataConTyCon dc))

        bitmap_size = d-s
        bitmap_size' :: Int
        bitmap_size' = fromIntegral bitmap_size
	bitmap = intsToReverseBitmap bitmap_size'
                        (sortLe (<=) (filter (< bitmap_size') rel_slots))
	  where
	  binds = Map.toList p
	  rel_slots = map fromIntegral $ concat (map spread binds)
	  spread (id, offset)
		| isFollowableArg (idCgRep id) = [ rel_offset ]
		| otherwise = []
		where rel_offset = d - offset - 1

     in do
     alt_stuff <- mapM codeAlt alts
     alt_final <- mkMultiBranch maybe_ncons alt_stuff

     let 
         alt_bco_name = getName bndr
         alt_bco = mkProtoBCO alt_bco_name alt_final (Left alts)
			0 bitmap_size bitmap True
     scrut_code <- schemeE (d + ret_frame_sizeW) (d + ret_frame_sizeW) p scrut
     alt_bco' <- emitBc alt_bco
     let push_alts
	    | isAlgCase = PUSH_ALTS alt_bco'
	    | otherwise = PUSH_ALTS_UNLIFTED alt_bco' (typeCgRep bndr_ty)
     return (push_alts `consOL` scrut_code)

generateCCall :: Word16 -> Sequel
              -> BCEnv
              -> CCallSpec
              -> Id
              -> [AnnExpr' Id VarSet]
              -> BcM BCInstrList

generateCCall d0 s p (CCallSpec target cconv safety) fn args_r_to_l
   = let 
         addr_sizeW :: Word16
         addr_sizeW = fromIntegral (cgRepSizeW NonPtrArg)

         pargs _ [] = return []
         pargs d (a:az) 
            = let arg_ty = repType (exprType (deAnnotate' a))

              in case splitTyConApp_maybe arg_ty of
		    Just (t, _)
		     | t == arrayPrimTyCon || t == mutableArrayPrimTyCon
                       -> do rest <- pargs (d + addr_sizeW) az
                             code <- parg_ArrayishRep (fromIntegral arrPtrsHdrSize) d p a
                             return ((code,AddrRep):rest)

		     | t == byteArrayPrimTyCon || t == mutableByteArrayPrimTyCon
                       -> do rest <- pargs (d + addr_sizeW) az
                             code <- parg_ArrayishRep (fromIntegral arrWordsHdrSize) d p a
                             return ((code,AddrRep):rest)

                    _
                       -> do (code_a, sz_a) <- pushAtom d p a
                             rest <- pargs (d+sz_a) az
                             return ((code_a, atomPrimRep a) : rest)

         parg_ArrayishRep :: Word16 -> Word16 -> BCEnv -> AnnExpr' Id VarSet
                          -> BcM BCInstrList
         parg_ArrayishRep hdrSize d p a
            = do (push_fo, _) <- pushAtom d p a
                 return (push_fo `snocOL` SWIZZLE 0 hdrSize)

     in do
     code_n_reps <- pargs d0 args_r_to_l
     let
         (pushs_arg, a_reps_pushed_r_to_l) = unzip code_n_reps
         a_reps_sizeW = fromIntegral (sum (map primRepSizeW a_reps_pushed_r_to_l))

         push_args    = concatOL pushs_arg
         d_after_args = d0 + a_reps_sizeW
         a_reps_pushed_RAW
            | null a_reps_pushed_r_to_l || head a_reps_pushed_r_to_l /= VoidRep
            = panic "ByteCodeGen.generateCCall: missing or invalid World token?"
            | otherwise
            = reverse (tail a_reps_pushed_r_to_l)

         (returns_void, r_rep)
            = case maybe_getCCallReturnRep (idType fn) of
                 Nothing -> (True,  VoidRep)
                 Just rr -> (False, rr) 
         
         get_target_info
            = case target of
                 DynamicTarget
                    -> return (False, panic "ByteCodeGen.generateCCall(dyn)")

                 StaticTarget target _
                    -> do res <- ioToBc (lookupStaticPtr stdcall_adj_target)
                          return (True, res)
                   where
                      stdcall_adj_target
#ifdef mingw32_TARGET_OS
                          | StdCallConv <- cconv
                          = let size = fromIntegral a_reps_sizeW * wORD_SIZE in
                            mkFastString (unpackFS target ++ '@':show size)
#endif
                          | otherwise
                          = target

     (is_static, static_target_addr) <- get_target_info
     let

         a_reps
                | is_static = a_reps_pushed_RAW
                | otherwise = if null a_reps_pushed_RAW 
                              then panic "ByteCodeGen.generateCCall: dyn with no args"
                              else tail a_reps_pushed_RAW

         (push_Addr, d_after_Addr)
            | is_static
            = (toOL [PUSH_UBX (Right static_target_addr) addr_sizeW],
               d_after_args + addr_sizeW)
            | otherwise
            = (nilOL, d_after_args)

         r_sizeW   = fromIntegral (primRepSizeW r_rep)
         d_after_r = d_after_Addr + r_sizeW
         r_lit     = mkDummyLiteral r_rep
         push_r    = (if   returns_void 
                      then nilOL 
                      else unitOL (PUSH_UBX (Left r_lit) r_sizeW))

	 stk_offset   = d_after_r - s

     token <- ioToBc $ prepForeignCall cconv a_reps r_rep
     let addr_of_marshaller = castPtrToFunPtr token

     recordItblMallocBc (ItblPtr (castFunPtrToPtr addr_of_marshaller))
     let
         do_call      = unitOL (CCALL stk_offset (castFunPtrToPtr addr_of_marshaller)
                                 (fromIntegral (fromEnum (playInterruptible safety))))
         wrapup       = mkSLIDE r_sizeW (d_after_r - r_sizeW - s)
                        `snocOL` RETURN_UBX (primRepToCgRep r_rep)
     return (
         push_args `appOL`
         push_Addr `appOL` push_r `appOL` do_call `appOL` wrapup
         )

mkDummyLiteral :: PrimRep -> Literal
mkDummyLiteral pr
   = case pr of
        IntRep    -> MachInt 0
        WordRep   -> MachWord 0
        AddrRep   -> MachNullAddr
        DoubleRep -> MachDouble 0
        FloatRep  -> MachFloat 0
        Int64Rep  -> MachInt64 0
        Word64Rep -> MachWord64 0
        _         -> panic "mkDummyLiteral"

maybe_getCCallReturnRep :: Type -> Maybe PrimRep
maybe_getCCallReturnRep fn_ty
   = let (_a_tys, r_ty) = splitFunTys (dropForAlls fn_ty)
         maybe_r_rep_to_go  
            = if isSingleton r_reps then Nothing else Just (r_reps !! 1)
         (r_tycon, r_reps) 
            = case splitTyConApp_maybe (repType r_ty) of
                      (Just (tyc, tys)) -> (tyc, map typePrimRep tys)
                      Nothing -> blargh
         ok = ( ( r_reps `lengthIs` 2 && VoidRep == head r_reps)
                || r_reps == [VoidRep] )
              && isUnboxedTupleTyCon r_tycon
              && case maybe_r_rep_to_go of
                    Nothing    -> True
                    Just r_rep -> r_rep /= PtrRep

         blargh :: a
         blargh = pprPanic "maybe_getCCallReturn: can't handle:" 
                           (pprType fn_ty)
     in 
     if ok then maybe_r_rep_to_go else blargh

implement_tagToId :: [Name] -> BcM BCInstrList
implement_tagToId names
   = ASSERT( notNull names )
     do labels <- getLabelsBc (genericLength names)
        label_fail <- getLabelBc
        label_exit <- getLabelBc
        let infos = zip4 labels (tail labels ++ [label_fail])
                                [0 ..] names
            steps = map (mkStep label_exit) infos
        return (concatOL steps
                  `appOL` 
                  toOL [LABEL label_fail, CASEFAIL, LABEL label_exit])
     where
        mkStep l_exit (my_label, next_label, n, name_for_n)
           = toOL [LABEL my_label, 
                   TESTEQ_I n next_label, 
                   PUSH_G name_for_n, 
                   JMP l_exit]

pushAtom :: Word16 -> BCEnv -> AnnExpr' Id VarSet -> BcM (BCInstrList, Word16)

pushAtom d p e 
   | Just e' <- bcView e 
   = pushAtom d p e'

pushAtom d p (AnnVar v)
   | idCgRep v == VoidArg
   = return (nilOL, 0)

   | isFCallId v
   = pprPanic "pushAtom: shouldn't get an FCallId here" (ppr v)

   | Just primop <- isPrimOpId_maybe v
   = return (unitOL (PUSH_PRIMOP primop), 1)

   | Just d_v <- lookupBCEnv_maybe v p
   = let l = d - d_v + sz - 2
     in return (toOL (genericReplicate sz (PUSH_L l)), sz)

    | otherwise
    = ASSERT(sz == 1) 
      return (unitOL (PUSH_G (getName v)), sz)

    where
         sz :: Word16
         sz = fromIntegral (idSizeW v)

pushAtom _ _ (AnnLit lit)
   = case lit of
        MachLabel _ _ _ -> code NonPtrArg
        MachWord _    -> code NonPtrArg
        MachInt _     -> code PtrArg
        MachFloat _   -> code FloatArg
        MachDouble _  -> code DoubleArg
        MachChar _    -> code NonPtrArg
	MachNullAddr  -> code NonPtrArg
        MachStr s     -> pushStr s
        l             -> pprPanic "pushAtom" (ppr l)
     where
        code rep
           = let size_host_words = fromIntegral (cgRepSizeW rep)
             in  return (unitOL (PUSH_UBX (Left lit) size_host_words), 
                           size_host_words)

        pushStr s 
           = let getMallocvilleAddr
                    = case s of
                         FastString _ n _ fp _ -> 
                                do ptr <- ioToBc (mallocBytes (n+1))
                                   recordMallocBc ptr
                                   ioToBc (
                                      withForeignPtr fp $ \p -> do
				         memcpy ptr p (fromIntegral n)
				         pokeByteOff ptr n (fromIntegral (ord '\0') :: Word8)
                                         return ptr
                                      )
             in do
                addr <- getMallocvilleAddr
                return (unitOL (PUSH_UBX (Right addr) 1), 1)

pushAtom d p (AnnCast e _)
   = pushAtom d p (snd e)

pushAtom _ _ expr
   = pprPanic "ByteCodeGen.pushAtom" 
              (pprCoreExpr (deAnnotate (undefined, expr)))

foreign import ccall unsafe "memcpy"
 memcpy :: Ptr a -> Ptr b -> CSize -> IO ()

mkMultiBranch :: Maybe Int
              -> [(Discr, BCInstrList)] 
              -> BcM BCInstrList
mkMultiBranch maybe_ncons raw_ways
   = let d_way     = filter (isNoDiscr.fst) raw_ways
         notd_ways = sortLe 
                        (\w1 w2 -> leAlt (fst w1) (fst w2))
                        (filter (not.isNoDiscr.fst) raw_ways)

         mkTree :: [(Discr, BCInstrList)] -> Discr -> Discr -> BcM BCInstrList
         mkTree [] _range_lo _range_hi = return the_default

         mkTree [val] range_lo range_hi
            | range_lo `eqAlt` range_hi 
            = return (snd val)
            | otherwise
            = do label_neq <- getLabelBc
                 return (testEQ (fst val) label_neq 
   			  `consOL` (snd val
   			  `appOL`   unitOL (LABEL label_neq)
                          `appOL`   the_default))

         mkTree vals range_lo range_hi
            = let n = length vals `div` 2
                  vals_lo = take n vals
                  vals_hi = drop n vals
                  v_mid = fst (head vals_hi)
              in do
              label_geq <- getLabelBc
              code_lo <- mkTree vals_lo range_lo (dec v_mid)
              code_hi <- mkTree vals_hi v_mid range_hi
              return (testLT v_mid label_geq
                        `consOL` (code_lo
			`appOL`   unitOL (LABEL label_geq)
			`appOL`   code_hi))
 
         the_default 
            = case d_way of [] -> unitOL CASEFAIL
                            [(_, def)] -> def
                            _ -> panic "mkMultiBranch/the_default"

         testLT (DiscrI i) fail_label = TESTLT_I i fail_label
         testLT (DiscrW i) fail_label = TESTLT_W i fail_label
         testLT (DiscrF i) fail_label = TESTLT_F i fail_label
         testLT (DiscrD i) fail_label = TESTLT_D i fail_label
         testLT (DiscrP i) fail_label = TESTLT_P i fail_label
         testLT NoDiscr    _          = panic "mkMultiBranch NoDiscr"

         testEQ (DiscrI i) fail_label = TESTEQ_I i fail_label
         testEQ (DiscrW i) fail_label = TESTEQ_W i fail_label
         testEQ (DiscrF i) fail_label = TESTEQ_F i fail_label
         testEQ (DiscrD i) fail_label = TESTEQ_D i fail_label
         testEQ (DiscrP i) fail_label = TESTEQ_P i fail_label
         testEQ NoDiscr    _          = panic "mkMultiBranch NoDiscr"

         (init_lo, init_hi)
            | null notd_ways
            = panic "mkMultiBranch: awesome foursome"
            | otherwise
            = case fst (head notd_ways) of
              	DiscrI _ -> ( DiscrI minBound, 	DiscrI maxBound )
              	DiscrW _ -> ( DiscrW minBound, 	DiscrW maxBound )
              	DiscrF _ -> ( DiscrF minF,     	DiscrF maxF )
              	DiscrD _ -> ( DiscrD minD,     	DiscrD maxD )
              	DiscrP _ -> ( DiscrP algMinBound, DiscrP algMaxBound )
              	NoDiscr -> panic "mkMultiBranch NoDiscr"

         (algMinBound, algMaxBound)
            = case maybe_ncons of
                 Just n  -> (0, fromIntegral n - 1)
                 Nothing -> (minBound, maxBound)

         (DiscrI i1) `eqAlt` (DiscrI i2) = i1 == i2
         (DiscrW w1) `eqAlt` (DiscrW w2) = w1 == w2
         (DiscrF f1) `eqAlt` (DiscrF f2) = f1 == f2
         (DiscrD d1) `eqAlt` (DiscrD d2) = d1 == d2
         (DiscrP i1) `eqAlt` (DiscrP i2) = i1 == i2
         NoDiscr     `eqAlt` NoDiscr     = True
         _           `eqAlt` _           = False

         (DiscrI i1) `leAlt` (DiscrI i2) = i1 <= i2
         (DiscrW w1) `leAlt` (DiscrW w2) = w1 <= w2
         (DiscrF f1) `leAlt` (DiscrF f2) = f1 <= f2
         (DiscrD d1) `leAlt` (DiscrD d2) = d1 <= d2
         (DiscrP i1) `leAlt` (DiscrP i2) = i1 <= i2
         NoDiscr     `leAlt` NoDiscr     = True
         _           `leAlt` _           = False

         isNoDiscr NoDiscr = True
         isNoDiscr _       = False

         dec (DiscrI i) = DiscrI (i-1)
         dec (DiscrW w) = DiscrW (w-1)
         dec (DiscrP i) = DiscrP (i-1)
         dec other      = other

         minF, maxF :: Float
         minD, maxD :: Double
         minF = -1.0e37
         maxF =  1.0e37
         minD = -1.0e308
         maxD =  1.0e308
     in
         mkTree notd_ways init_lo init_hi

data Discr 
   = DiscrI Int
   | DiscrW Word
   | DiscrF Float
   | DiscrD Double
   | DiscrP Word16
   | NoDiscr

instance Outputable Discr where
   ppr (DiscrI i) = int i
   ppr (DiscrW w) = text (show w)
   ppr (DiscrF f) = text (show f)
   ppr (DiscrD d) = text (show d)
   ppr (DiscrP i) = ppr i
   ppr NoDiscr    = text "DEF"

lookupBCEnv_maybe :: Id -> BCEnv -> Maybe Word16
lookupBCEnv_maybe = Map.lookup

idSizeW :: Id -> Int
idSizeW id = cgRepSizeW (typeCgRep (idType id))

unboxedTupleException :: a
unboxedTupleException 
   = ghcError 
        (ProgramError 
           ("Error: bytecode compiler can't handle unboxed tuples.\n"++
            "  Possibly due to foreign import/export decls in source.\n"++
            "  Workaround: use -fobject-code, or compile this module to .o separately."))

mkSLIDE :: Word16 -> Word16 -> OrdList BCInstr
mkSLIDE n d = if d == 0 then nilOL else unitOL (SLIDE n d)

splitApp :: AnnExpr' Var ann -> (AnnExpr' Var ann, [AnnExpr' Var ann])
splitApp e | Just e' <- bcView e = splitApp e'
splitApp (AnnApp (_,f) (_,a)) 	 = case splitApp f of 
			      	      (f', as) -> (f', a:as)
splitApp e		      	 = (e, [])

bcView :: AnnExpr' Var ann -> Maybe (AnnExpr' Var ann)
bcView (AnnNote _ (_,e)) 	     = Just e
bcView (AnnCast (_,e) _) 	     = Just e
bcView (AnnLam v (_,e)) | isTyCoVar v  = Just e
bcView (AnnApp (_,e) (_, AnnType _)) = Just e
bcView _                             = Nothing

isVoidArgAtom :: AnnExpr' Var ann -> Bool
isVoidArgAtom e | Just e' <- bcView e = isVoidArgAtom e'
isVoidArgAtom (AnnVar v)              = typePrimRep (idType v) == VoidRep
isVoidArgAtom _ 	              = False

atomPrimRep :: AnnExpr' Id ann -> PrimRep
atomPrimRep e | Just e' <- bcView e = atomPrimRep e'
atomPrimRep (AnnVar v)    	    = typePrimRep (idType v)
atomPrimRep (AnnLit l)    	    = typePrimRep (literalType l)
atomPrimRep other = pprPanic "atomPrimRep" (ppr (deAnnotate (undefined,other)))

atomRep :: AnnExpr' Id ann -> CgRep
atomRep e = primRepToCgRep (atomPrimRep e)

isPtrAtom :: AnnExpr' Id ann -> Bool
isPtrAtom e = atomRep e == PtrArg

mkStackOffsets :: Word16 -> [Word16] -> [Word16]
mkStackOffsets original_depth szsw
   = map (subtract 1) (tail (scanl (+) original_depth szsw))

type BcPtr = Either ItblPtr (Ptr ())

data BcM_State 
   = BcM_State { 
        uniqSupply :: UniqSupply,
	nextlabel :: Word16,
	malloced  :: [BcPtr],
        breakArray :: BreakArray
        }

newtype BcM r = BcM (BcM_State -> IO (BcM_State, r))

ioToBc :: IO a -> BcM a
ioToBc io = BcM $ \st -> do 
  x <- io 
  return (st, x)

runBc :: UniqSupply -> ModBreaks -> BcM r -> IO (BcM_State, r)
runBc us modBreaks (BcM m) 
   = m (BcM_State us 0 [] breakArray)   
   where
   breakArray = modBreaks_flags modBreaks

thenBc :: BcM a -> (a -> BcM b) -> BcM b
thenBc (BcM expr) cont = BcM $ \st0 -> do
  (st1, q) <- expr st0
  let BcM k = cont q 
  (st2, r) <- k st1
  return (st2, r)

thenBc_ :: BcM a -> BcM b -> BcM b
thenBc_ (BcM expr) (BcM cont) = BcM $ \st0 -> do
  (st1, _) <- expr st0
  (st2, r) <- cont st1
  return (st2, r)

returnBc :: a -> BcM a
returnBc result = BcM $ \st -> (return (st, result))

instance Monad BcM where
  (>>=) = thenBc
  (>>)  = thenBc_
  return = returnBc

emitBc :: ([BcPtr] -> ProtoBCO Name) -> BcM (ProtoBCO Name)
emitBc bco
  = BcM $ \st -> return (st{malloced=[]}, bco (malloced st))

recordMallocBc :: Ptr a -> BcM ()
recordMallocBc a
  = BcM $ \st -> return (st{malloced = Right (castPtr a) : malloced st}, ())

recordItblMallocBc :: ItblPtr -> BcM ()
recordItblMallocBc a
  = BcM $ \st -> return (st{malloced = Left a : malloced st}, ())

getLabelBc :: BcM Word16
getLabelBc
  = BcM $ \st -> do let nl = nextlabel st
                    when (nl == maxBound) $
                        panic "getLabelBc: Ran out of labels"
                    return (st{nextlabel = nl + 1}, nl)

getLabelsBc :: Word16 -> BcM [Word16]
getLabelsBc n
  = BcM $ \st -> let ctr = nextlabel st 
		 in return (st{nextlabel = ctr+n}, [ctr .. ctr+n-1])

getBreakArray :: BcM BreakArray 
getBreakArray = BcM $ \st -> return (st, breakArray st)

newUnique :: BcM Unique
newUnique = BcM $
   \st -> case takeUniqFromSupply (uniqSupply st) of
             (uniq, us) -> let newState = st { uniqSupply = us }
                           in  return (newState, uniq)

newId :: Type -> BcM Id
newId ty = do 
    uniq <- newUnique
    return $ mkSysLocal tickFS uniq ty

tickFS :: FastString
tickFS = fsLit "ticked"
