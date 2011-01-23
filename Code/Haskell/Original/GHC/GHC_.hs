module GHC (
	defaultErrorHandler,
	defaultCleanupHandler,

        Ghc, GhcT, GhcMonad(..),
        runGhc, runGhcT, initGhcMonad,
        gcatch, gbracket, gfinally,
        printException,
        printExceptionAndWarnings,
        handleSourceError,
        needsTemplateHaskell,

	DynFlags(..), DynFlag(..), Severity(..), HscTarget(..), dopt,
        GhcMode(..), GhcLink(..), defaultObjectTarget,
	parseDynamicFlags,
	getSessionDynFlags,
	setSessionDynFlags,
	parseStaticFlags,

	Target(..), TargetId(..), Phase,
	setTargets,
	getTargets,
	addTarget,
	removeTarget,
	guessTarget,
	
	depanal,
	load, LoadHowMuch(..),
	SuccessFlag(..), succeeded, failed,
        defaultWarnErrLogger, WarnErrLogger,
	workingDirectoryChanged,
        parseModule, typecheckModule, desugarModule, loadModule,
        ParsedModule(..), TypecheckedModule(..), DesugaredModule(..),
	TypecheckedSource, ParsedSource, RenamedSource,
        TypecheckedMod, ParsedMod,
        moduleInfo, renamedSource, typecheckedSource,
        parsedSource, coreModule,
        compileToCoreModule, compileToCoreSimplified,
        compileCoreToObj,
        getModSummary,

	ModuleGraph, ModSummary(..), ms_mod_name, ModLocation(..),
	getModuleGraph,
	isLoaded,
	topSortModuleGraph,

	ModuleInfo,
	getModuleInfo,
	modInfoTyThings,
	modInfoTopLevelScope,
        modInfoExports,
	modInfoInstances,
	modInfoIsExportedName,
	modInfoLookupName,
	lookupGlobalName,
	findGlobalAnns,
        mkPrintUnqualifiedForModule,

        packageDbModules,

	PrintUnqualified, alwaysQualify,

	getBindings, getPrintUnqual,
        findModule,
        lookupModule,
#ifdef GHCI
	setContext, getContext,	
	getNamesInScope,
	getRdrNamesInScope,
        getGRE,
	moduleIsInterpreted,
	getInfo,
	exprType,
	typeKind,
	parseName,
	RunResult(..),  
	runStmt, parseImportDecl, SingleStep(..),
        resume,
        Resume(resumeStmt, resumeThreadId, resumeBreakInfo, resumeSpan,
               resumeHistory, resumeHistoryIx),
        History(historyBreakInfo, historyEnclosingDecls), 
        GHC.getHistorySpan, getHistoryModule,
        getResumeContext,
        abandon, abandonAll,
        InteractiveEval.back,
        InteractiveEval.forward,
	showModule,
        isModuleInterpreted,
	InteractiveEval.compileExpr, HValue, dynCompileExpr,
        GHC.obtainTermFromId, GHC.obtainTermFromVal, reconstructType,
        modInfoModBreaks,
        ModBreaks(..), BreakIndex,
        BreakInfo(breakInfo_number, breakInfo_module),
        BreakArray, setBreakOn, setBreakOff, getBreak,
#endif
        lookupName,

        PackageId,

	Module, mkModule, pprModule, moduleName, modulePackageId,
        ModuleName, mkModuleName, moduleNameString,

	Name, 
	isExternalName, nameModule, pprParenSymName, nameSrcSpan,
	NamedThing(..),
	RdrName(Qual,Unqual),
	
	Id, idType,
	isImplicitId, isDeadBinder,
	isExportedId, isLocalId, isGlobalId,
	isRecordSelector,
	isPrimOpId, isFCallId, isClassOpId_maybe,
	isDataConWorkId, idDataCon,
	isBottomingId, isDictonaryId,
	recordSelectorFieldLabel,

	TyCon, 
	tyConTyVars, tyConDataCons, tyConArity,
	isClassTyCon, isSynTyCon, isNewTyCon, isPrimTyCon, isFunTyCon,
	isFamilyTyCon,
	synTyConDefn, synTyConType, synTyConResKind,

	TyVar,
	alphaTyVars,

	DataCon,
	dataConSig, dataConType, dataConTyCon, dataConFieldLabels,
	dataConIsInfix, isVanillaDataCon, dataConUserType,
	dataConStrictMarks,  
	StrictnessMark(..), isMarkedStrict,

	Class, 
	classMethods, classSCTheta, classTvsFds,
	pprFundeps,

	Instance, 
	instanceDFunId, pprInstance, pprInstanceHdr,

	Type, splitForAllTys, funResultTy, 
	pprParendType, pprTypeApp, 
	Kind,
	PredType,
	ThetaType, pprForAll, pprThetaArrow,

	TyThing(..), 

	module HsSyn,

	FixityDirection(..), 
	defaultFixity, maxPrecedence, 
	negateFixity,
	compareFixity,

	SrcLoc, pprDefnLoc,
        mkSrcLoc, isGoodSrcLoc, noSrcLoc,
	srcLocFile, srcLocLine, srcLocCol,
        SrcSpan,
        mkSrcSpan, srcLocSpan, isGoodSrcSpan, noSrcSpan,
        srcSpanStart, srcSpanEnd,
	srcSpanFile, 
        srcSpanStartLine, srcSpanEndLine, 
        srcSpanStartCol, srcSpanEndCol,

	Located(..),

	noLoc, mkGeneralLocated,

	getLoc, unLoc,

	eqLocated, cmpLocated, combineLocs, addCLoc,
        leftmost_smallest, leftmost_largest, rightmost,
        spans, isSubspanOf,

	GhcException(..), showGhcException,

        Token,
        getTokenStream, getRichTokenStream,
        showRichTokenStream, addSourceToTokens,

        parser,

	cyclicModuleErr,
  ) where

#include "HsVersions.h"

#ifdef GHCI
import qualified Linker
import Linker           ( HValue )
import ByteCodeInstr
import BreakArray
import InteractiveEval
#endif

import HscMain
import DriverPipeline
import GhcMonad
import TcIface          ( typecheckIface )
import TcRnTypes
import TcRnMonad        ( initIfaceCheck )
import Packages
import NameSet
import RdrName
import qualified HsSyn
import HsSyn hiding ((<.>))
import Type
import Coercion		( synTyConResKind )
import TcType		hiding( typeKind )
import Id
import Var
import TysPrim		( alphaTyVars )
import TyCon
import Class
import DataCon
import Name             hiding ( varName )
import InstEnv
import SrcLoc
import CoreSyn          ( CoreBind )
import TidyPgm
import DriverPhases     ( Phase(..), isHaskellSrcFilename, startPhase )
import HeaderInfo
import Finder
import HscTypes
import DynFlags
import StaticFlagParser
import qualified StaticFlags
import SysTools     ( initSysTools, cleanTempFiles, cleanTempFilesExcept,
                      cleanTempDirs )
import Annotations
import Module
import UniqFM
import Panic
import Digraph
import Bag		( unitBag, listToBag )
import ErrUtils
import MonadUtils
import Util
import StringBuffer
import Outputable
import BasicTypes
import Maybes		( expectJust, mapCatMaybes )
import FastString
import qualified Parser
import Lexer

import System.Directory ( getModificationTime, doesFileExist,
                          getCurrentDirectory )
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import qualified FiniteMap as Map
import Data.List
import qualified Data.List as List
import Data.Typeable    ( Typeable )
import Data.Word        ( Word8 )
import Control.Monad
import System.Exit	( exitWith, ExitCode(..) )
import System.Time	( ClockTime, getClockTime )
import Exception
import Data.IORef
import System.FilePath
import System.IO
import System.IO.Error	( isDoesNotExistError )
import Prelude hiding (init)

defaultErrorHandler :: (ExceptionMonad m, MonadIO m) => DynFlags -> m a -> m a
defaultErrorHandler dflags inner =
  ghandle (\exception -> liftIO $ do
           hFlush stdout
           case fromException exception of
                Just (ioe :: IOException) ->
                  fatalErrorMsg dflags (text (show ioe))
                _ -> case fromException exception of
		     Just UserInterrupt -> exitWith (ExitFailure 1)
                     Just StackOverflow ->
                         fatalErrorMsg dflags (text "stack overflow: use +RTS -K<size> to increase it")
                     _ -> case fromException exception of
                          Just (ex :: ExitCode) -> throw ex
                          _ ->
                              fatalErrorMsg dflags
                                  (text (show (Panic (show exception))))
           exitWith (ExitFailure 1)
         ) $

  handleGhcException
            (\ge -> liftIO $ do
  		hFlush stdout
  		case ge of
		     PhaseFailed _ code -> exitWith code
		     Signal _ -> exitWith (ExitFailure 1)
		     _ -> do fatalErrorMsg dflags (text (show ge))
			     exitWith (ExitFailure 1)
	    ) $
  inner

defaultCleanupHandler :: (ExceptionMonad m, MonadIO m) =>
                         DynFlags -> m a -> m a
defaultCleanupHandler dflags inner =
    inner `gfinally`
          (liftIO $ do
              cleanTempFiles dflags
              cleanTempDirs dflags
          )

printException :: GhcMonad m => SourceError -> m ()
printException err = do
  dflags <- getSessionDynFlags
  liftIO $ printBagOfErrors dflags (srcErrorMessages err)

{-# DEPRECATED printExceptionAndWarnings "use printException instead" #-}
printExceptionAndWarnings :: GhcMonad m => SourceError -> m ()
printExceptionAndWarnings = printException

runGhc :: Maybe FilePath
       -> Ghc a
       -> IO a
runGhc mb_top_dir ghc = do
  ref <- newIORef undefined
  let session = Session ref
  flip unGhc session $ do
    initGhcMonad mb_top_dir
    ghc

runGhcT :: (ExceptionMonad m, Functor m, MonadIO m) =>
           Maybe FilePath
        -> GhcT m a
        -> m a
runGhcT mb_top_dir ghct = do
  ref <- liftIO $ newIORef undefined
  let session = Session ref
  flip unGhcT session $ do
    initGhcMonad mb_top_dir
    ghct

initGhcMonad :: GhcMonad m => Maybe FilePath -> m ()
initGhcMonad mb_top_dir = do
  liftIO $ installSignalHandlers

  liftIO $ StaticFlags.initStaticOpts

  dflags0 <- liftIO $ initDynFlags defaultDynFlags
  dflags <- liftIO $ initSysTools mb_top_dir dflags0
  env <- liftIO $ newHscEnv dflags
  setSession env

setSessionDynFlags :: GhcMonad m => DynFlags -> m [PackageId]
setSessionDynFlags dflags = do
  (dflags', preload) <- liftIO $ initPackages dflags
  modifySession (\h -> h{ hsc_dflags = dflags' })
  return preload

guessOutputFile :: GhcMonad m => m ()
guessOutputFile = modifySession $ \env ->
    let dflags = hsc_dflags env
        mod_graph = hsc_mod_graph env
        mainModuleSrcPath :: Maybe String
        mainModuleSrcPath = do
            let isMain = (== mainModIs dflags) . ms_mod
            [ms] <- return (filter isMain mod_graph)
            ml_hs_file (ms_location ms)
        name = fmap dropExtension mainModuleSrcPath

#if defined(mingw32_HOST_OS)
        name_exe = fmap (<.> "exe") name
#else
        name_exe = name
#endif
    in
    case outputFile dflags of
        Just _ -> env
        Nothing -> env { hsc_dflags = dflags { outputFile = name_exe } }

setTargets :: GhcMonad m => [Target] -> m ()
setTargets targets = modifySession (\h -> h{ hsc_targets = targets })

getTargets :: GhcMonad m => m [Target]
getTargets = withSession (return . hsc_targets)

addTarget :: GhcMonad m => Target -> m ()
addTarget target
  = modifySession (\h -> h{ hsc_targets = target : hsc_targets h })

removeTarget :: GhcMonad m => TargetId -> m ()
removeTarget target_id
  = modifySession (\h -> h{ hsc_targets = filter (hsc_targets h) })
  where
   filter targets = [ t | t@(Target id _ _) <- targets, id /= target_id ]

guessTarget :: GhcMonad m => String -> Maybe Phase -> m Target
guessTarget str (Just phase)
   = return (Target (TargetFile str (Just phase)) True Nothing)
guessTarget str Nothing
   | isHaskellSrcFilename file
   = return (target (TargetFile file Nothing))
   | otherwise
   = do exists <- liftIO $ doesFileExist hs_file
	if exists
	   then return (target (TargetFile hs_file Nothing))
	   else do
	exists <- liftIO $ doesFileExist lhs_file
	if exists
	   then return (target (TargetFile lhs_file Nothing))
	   else do
        if looksLikeModuleName file
           then return (target (TargetModule (mkModuleName file)))
           else do
        throwGhcException
                 (ProgramError (showSDoc $
                 text "target" <+> quotes (text file) <+> 
                 text "is not a module name or a source file"))
     where 
         (file,obj_allowed)
                | '*':rest <- str = (rest, False)
                | otherwise       = (str,  True)

	 hs_file  = file <.> "hs"
	 lhs_file = file <.> "lhs"

         target tid = Target tid obj_allowed Nothing

depanal :: GhcMonad m =>
           [ModuleName]
        -> Bool
        -> m ModuleGraph
depanal excluded_mods allow_dup_roots = do
  hsc_env <- getSession
  let
	 dflags  = hsc_dflags hsc_env
	 targets = hsc_targets hsc_env
	 old_graph = hsc_mod_graph hsc_env
	
  liftIO $ showPass dflags "Chasing dependencies"
  liftIO $ debugTraceMsg dflags 2 (hcat [
	     text "Chasing modules from: ",
	     hcat (punctuate comma (map pprTarget targets))])

  mod_graph <- liftIO $ downsweep hsc_env old_graph excluded_mods allow_dup_roots
  modifySession $ \_ -> hsc_env { hsc_mod_graph = mod_graph }
  return mod_graph

data LoadHowMuch
   = LoadAllTargets
   | LoadUpTo ModuleName
   | LoadDependenciesOf ModuleName

load :: GhcMonad m => LoadHowMuch -> m SuccessFlag
load how_much = do
   mod_graph <- depanal [] False
   load2 how_much mod_graph

type WarnErrLogger = GhcMonad m => Maybe SourceError -> m ()

defaultWarnErrLogger :: WarnErrLogger
defaultWarnErrLogger Nothing  = return ()
defaultWarnErrLogger (Just e) = printException e

load2 :: GhcMonad m => LoadHowMuch -> [ModSummary]
      -> m SuccessFlag
load2 how_much mod_graph = do
        guessOutputFile
	hsc_env <- getSession

        let hpt1      = hsc_HPT hsc_env
        let dflags    = hsc_dflags hsc_env

        let all_home_mods = [ms_mod_name s 
			    | s <- mod_graph, not (isBootSummary s)]
	    bad_boot_mods = [s 	      | s <- mod_graph, isBootSummary s,
					not (ms_mod_name s `elem` all_home_mods)]
	ASSERT( null bad_boot_mods ) return ()

        let checkHowMuch (LoadUpTo m)           = checkMod m
            checkHowMuch (LoadDependenciesOf m) = checkMod m
            checkHowMuch _ = id

            checkMod m and_then
                | m `elem` all_home_mods = and_then
                | otherwise = do 
                        liftIO $ errorMsg dflags (text "no such module:" <+>
                                         quotes (ppr m))
                        return Failed

        checkHowMuch how_much $ do

        let mg2_with_srcimps :: [SCC ModSummary]
	    mg2_with_srcimps = topSortModuleGraph True mod_graph Nothing

	warnUnnecessarySourceImports mg2_with_srcimps

 	let
	    stable_mods@(stable_obj,stable_bco)
	        = checkStability hpt1 mg2_with_srcimps all_home_mods

	    pruned_hpt = pruneHomePackageTable hpt1 
				(flattenSCCs mg2_with_srcimps)
				stable_mods

	_ <- liftIO $ evaluate pruned_hpt

        modifySession $ \_ -> hsc_env{ hsc_IC = emptyInteractiveContext,
                                       hsc_HPT = pruned_hpt }

	liftIO $ debugTraceMsg dflags 2 (text "Stable obj:" <+> ppr stable_obj $$
				text "Stable BCO:" <+> ppr stable_bco)

	let stable_linkables = [ linkable
			       | m <- stable_obj++stable_bco,
				 Just hmi <- [lookupUFM pruned_hpt m],
				 Just linkable <- [hm_linkable hmi] ]
	liftIO $ unload hsc_env stable_linkables

        let full_mg :: [SCC ModSummary]
	    full_mg    = topSortModuleGraph False mod_graph Nothing

	    maybe_top_mod = case how_much of
				LoadUpTo m           -> Just m
			  	LoadDependenciesOf m -> Just m
			  	_		     -> Nothing

	    partial_mg0 :: [SCC ModSummary]
	    partial_mg0 = topSortModuleGraph False mod_graph maybe_top_mod

	    partial_mg
		| LoadDependenciesOf _mod <- how_much
		= ASSERT( case last partial_mg0 of 
			    AcyclicSCC ms -> ms_mod_name ms == _mod; _ -> False )
		  List.init partial_mg0
		| otherwise
		= partial_mg0
  
	    stable_mg = 
		[ AcyclicSCC ms
	        | AcyclicSCC ms <- full_mg,
		  ms_mod_name ms `elem` stable_obj++stable_bco,
		  ms_mod_name ms `notElem` [ ms_mod_name ms' | 
						AcyclicSCC ms' <- partial_mg ] ]

	    mg = stable_mg ++ partial_mg

	let cleanup = cleanTempFilesExcept dflags
			  (ppFilesFromSummaries (flattenSCCs mg2_with_srcimps))

	liftIO $ debugTraceMsg dflags 2 (hang (text "Ready for upsweep")
				   2 (ppr mg))

        setSession hsc_env{ hsc_HPT = emptyHomePackageTable }
        (upsweep_ok, modsUpswept)
           <- upsweep pruned_hpt stable_mods cleanup mg

        let modsDone = reverse modsUpswept

        if succeeded upsweep_ok

         then 
           do liftIO $ debugTraceMsg dflags 2 (text "Upsweep completely successful.")

	      liftIO $ cleanTempFilesExcept dflags (ppFilesFromSummaries modsDone)

	      let ofile = outputFile dflags
	      let no_hs_main = dopt Opt_NoHsMain dflags
	      let 
	 	main_mod = mainModIs dflags
		a_root_is_Main = any ((==main_mod).ms_mod) mod_graph
		do_linking = a_root_is_Main || no_hs_main || ghcLink dflags == LinkDynLib

	      when (ghcLink dflags == LinkBinary 
                    && isJust ofile && not do_linking) $
	        liftIO $ debugTraceMsg dflags 1 $
                    text ("Warning: output was redirected with -o, " ++
                          "but no output will be generated\n" ++
			  "because there is no " ++ 
                          moduleNameString (moduleName main_mod) ++ " module.")

              hsc_env1 <- getSession
              linkresult <- liftIO $ link (ghcLink dflags) dflags do_linking (hsc_HPT hsc_env1)

	      loadFinish Succeeded linkresult

         else 
           do liftIO $ debugTraceMsg dflags 2 (text "Upsweep partially successful.")

              let modsDone_names
                     = map ms_mod modsDone
              let mods_to_zap_names 
                     = findPartiallyCompletedCycles modsDone_names 
			  mg2_with_srcimps
              let mods_to_keep
                     = filter ((`notElem` mods_to_zap_names).ms_mod) 
			  modsDone

              hsc_env1 <- getSession
              let hpt4 = retainInTopLevelEnvs (map ms_mod_name mods_to_keep) 
					      (hsc_HPT hsc_env1)

	      liftIO $ cleanTempFilesExcept dflags (ppFilesFromSummaries mods_to_keep)

	      ASSERT(all (isJust.hm_linkable) 
			(eltsUFM (hsc_HPT hsc_env))) do
	
              linkresult <- liftIO $ link (ghcLink dflags) dflags False hpt4

              modifySession $ \hsc_env -> hsc_env{ hsc_HPT = hpt4 }
	      loadFinish Failed linkresult

loadFinish :: GhcMonad m =>
              SuccessFlag -> SuccessFlag
           -> m SuccessFlag
loadFinish _all_ok Failed
  = do hsc_env <- getSession
       liftIO $ unload hsc_env []
       modifySession discardProg
       return Failed

loadFinish all_ok Succeeded
  = do modifySession $ \hsc_env -> hsc_env{ hsc_IC = emptyInteractiveContext }
       return all_ok

discardProg :: HscEnv -> HscEnv
discardProg hsc_env
  = hsc_env { hsc_mod_graph = emptyMG, 
	      hsc_IC = emptyInteractiveContext,
	      hsc_HPT = emptyHomePackageTable }

ppFilesFromSummaries :: [ModSummary] -> [FilePath]
ppFilesFromSummaries summaries = map ms_hspp_file summaries

class ParsedMod m where
  modSummary   :: m -> ModSummary
  parsedSource :: m -> ParsedSource

class ParsedMod m => TypecheckedMod m where
  renamedSource     :: m -> Maybe RenamedSource
  typecheckedSource :: m -> TypecheckedSource
  moduleInfo        :: m -> ModuleInfo
  tm_internals      :: m -> (TcGblEnv, ModDetails)

class TypecheckedMod m => DesugaredMod m where
  coreModule :: m -> ModGuts

data ParsedModule =
  ParsedModule { pm_mod_summary   :: ModSummary
               , pm_parsed_source :: ParsedSource }

instance ParsedMod ParsedModule where
  modSummary m    = pm_mod_summary m
  parsedSource m = pm_parsed_source m

data TypecheckedModule =
  TypecheckedModule { tm_parsed_module       :: ParsedModule
                    , tm_renamed_source      :: Maybe RenamedSource
                    , tm_typechecked_source  :: TypecheckedSource
                    , tm_checked_module_info :: ModuleInfo
                    , tm_internals_          :: (TcGblEnv, ModDetails)
                    }

instance ParsedMod TypecheckedModule where
  modSummary m   = modSummary (tm_parsed_module m)
  parsedSource m = parsedSource (tm_parsed_module m)

instance TypecheckedMod TypecheckedModule where
  renamedSource m     = tm_renamed_source m
  typecheckedSource m = tm_typechecked_source m
  moduleInfo m = tm_checked_module_info m
  tm_internals m      = tm_internals_ m

data DesugaredModule =
  DesugaredModule { dm_typechecked_module :: TypecheckedModule
                  , dm_core_module        :: ModGuts
             }

instance ParsedMod DesugaredModule where
  modSummary m   = modSummary (dm_typechecked_module m)
  parsedSource m = parsedSource (dm_typechecked_module m)

instance TypecheckedMod DesugaredModule where
  renamedSource m     = renamedSource (dm_typechecked_module m)
  typecheckedSource m = typecheckedSource (dm_typechecked_module m)
  moduleInfo m        = moduleInfo (dm_typechecked_module m)
  tm_internals m      = tm_internals_ (dm_typechecked_module m)

instance DesugaredMod DesugaredModule where
  coreModule m = dm_core_module m

type ParsedSource      = Located (HsModule RdrName)
type RenamedSource     = (HsGroup Name, [LImportDecl Name], Maybe [LIE Name],
                          Maybe LHsDocString)
type TypecheckedSource = LHsBinds Id

getModSummary :: GhcMonad m => ModuleName -> m ModSummary
getModSummary mod = do
   mg <- liftM hsc_mod_graph getSession
   case [ ms | ms <- mg, ms_mod_name ms == mod, not (isBootSummary ms) ] of
     [] -> throw $ mkApiErr (text "Module not part of module graph")
     [ms] -> return ms
     multiple -> throw $ mkApiErr (text "getModSummary is ambiguous: " <+> ppr multiple)

parseModule :: GhcMonad m => ModSummary -> m ParsedModule
parseModule ms = do
   hsc_env <- getSession
   let hsc_env_tmp = hsc_env { hsc_dflags = ms_hspp_opts ms }
   rdr_module <- liftIO $ hscParse hsc_env_tmp ms
   return (ParsedModule ms rdr_module)

typecheckModule :: GhcMonad m => ParsedModule -> m TypecheckedModule
typecheckModule pmod = do
 let ms = modSummary pmod
 hsc_env <- getSession
 let hsc_env_tmp = hsc_env { hsc_dflags = ms_hspp_opts ms }
 (tc_gbl_env, rn_info)
       <- liftIO $ hscTypecheckRename hsc_env_tmp ms (parsedSource pmod)
 details <- liftIO $ makeSimpleDetails hsc_env_tmp tc_gbl_env
 return $
     TypecheckedModule {
       tm_internals_          = (tc_gbl_env, details),
       tm_parsed_module       = pmod,
       tm_renamed_source      = rn_info,
       tm_typechecked_source  = tcg_binds tc_gbl_env,
       tm_checked_module_info =
         ModuleInfo {
           minf_type_env  = md_types details,
           minf_exports   = availsToNameSet $ md_exports details,
           minf_rdr_env   = Just (tcg_rdr_env tc_gbl_env),
           minf_instances = md_insts details
#ifdef GHCI
           ,minf_modBreaks = emptyModBreaks
#endif
         }}

desugarModule :: GhcMonad m => TypecheckedModule -> m DesugaredModule
desugarModule tcm = do
 let ms = modSummary tcm
 let (tcg, _) = tm_internals tcm
 hsc_env <- getSession
 let hsc_env_tmp = hsc_env { hsc_dflags = ms_hspp_opts ms }
 guts <- liftIO $ hscDesugar hsc_env_tmp ms tcg
 return $
     DesugaredModule {
       dm_typechecked_module = tcm,
       dm_core_module        = guts
     }

loadModule :: (TypecheckedMod mod, GhcMonad m) => mod -> m mod
loadModule tcm = do
   let ms = modSummary tcm
   let mod = ms_mod_name ms
   let loc = ms_location ms
   let (tcg, _details) = tm_internals tcm

   mb_linkable <- case ms_obj_date ms of
                     Just t | t > ms_hs_date ms  -> do
                         l <- liftIO $ findObjectLinkable (ms_mod ms) 
                                                  (ml_obj_file loc) t
                         return (Just l)
                     _otherwise -> return Nothing
                                                
   hsc_env <- getSession
   mod_info <- liftIO $ compile' (hscNothingBackendOnly     tcg,
                                  hscInteractiveBackendOnly tcg,
                                  hscBatchBackendOnly       tcg)
                                  hsc_env ms 1 1 Nothing mb_linkable

   modifySession $ \e -> e{ hsc_HPT = addToUFM (hsc_HPT e) mod mod_info }
   return tcm

data CoreModule
  = CoreModule {
      cm_module   :: !Module,
      cm_types    :: !TypeEnv,
      cm_binds    :: [CoreBind],
      cm_imports  :: ![Module]
    }

instance Outputable CoreModule where
   ppr (CoreModule {cm_module = mn, cm_types = te, cm_binds = cb}) =
      text "%module" <+> ppr mn <+> ppr te $$ vcat (map ppr cb)

compileToCoreModule :: GhcMonad m => FilePath -> m CoreModule
compileToCoreModule = compileCore False

compileToCoreSimplified :: GhcMonad m => FilePath -> m CoreModule
compileToCoreSimplified = compileCore True

compileCoreToObj :: GhcMonad m => Bool -> CoreModule -> m ()
compileCoreToObj simplify cm@(CoreModule{ cm_module = mName }) = do
  dflags      <- getSessionDynFlags
  currentTime <- liftIO $ getClockTime
  cwd         <- liftIO $ getCurrentDirectory
  modLocation <- liftIO $ mkHiOnlyModLocation dflags (hiSuf dflags) cwd
                   ((moduleNameSlashes . moduleName) mName)

  let modSummary = ModSummary { ms_mod = mName,
         ms_hsc_src = ExtCoreFile,
         ms_location = modLocation,
         ms_hs_date = currentTime,
         ms_obj_date = Nothing,
         ms_srcimps = [],
         ms_imps = [],
         ms_hspp_file = "",
         ms_hspp_opts = dflags,
         ms_hspp_buf = Nothing
      }

  hsc_env <- getSession
  liftIO $ hscCompileCore hsc_env simplify modSummary (cm_binds cm)

compileCore :: GhcMonad m => Bool -> FilePath -> m CoreModule
compileCore simplify fn = do
   target <- guessTarget fn Nothing
   addTarget target
   _ <- load LoadAllTargets
   modGraph <- depanal [] True
   case find ((== fn) . msHsFilePath) modGraph of
     Just modSummary -> do
       mod_guts <- coreModule `fmap`
                      (desugarModule =<< typecheckModule =<< parseModule modSummary)
       liftM gutsToCoreModule $
         if simplify
          then do
             hsc_env <- getSession
             simpl_guts <- liftIO $ hscSimplify hsc_env mod_guts
             tidy_guts <- liftIO $ tidyProgram hsc_env simpl_guts
             return $ Left tidy_guts
          else
             return $ Right mod_guts

     Nothing -> panic "compileToCoreModule: target FilePath not found in\
                           module dependency graph"
  where
        gutsToCoreModule :: Either (CgGuts, ModDetails) ModGuts -> CoreModule
        gutsToCoreModule (Left (cg, md))  = CoreModule {
          cm_module = cg_module cg,    cm_types = md_types md,
          cm_imports = cg_dir_imps cg, cm_binds = cg_binds cg
        }
        gutsToCoreModule (Right mg) = CoreModule {
          cm_module  = mg_module mg,                   cm_types   = mg_types mg,
          cm_imports = moduleEnvKeys (mg_dir_imps mg), cm_binds   = mg_binds mg
         }

unload :: HscEnv -> [Linkable] -> IO ()
unload hsc_env stable_linkables
  = case ghcLink (hsc_dflags hsc_env) of
#ifdef GHCI
	LinkInMemory -> Linker.unload (hsc_dflags hsc_env) stable_linkables
#else
	LinkInMemory -> panic "unload: no interpreter"
                                hsc_env stable_linkables
#endif
	_other -> return ()

checkStability
	:: HomePackageTable
	-> [SCC ModSummary]
	-> [ModuleName]
	-> ([ModuleName],
	    [ModuleName])

checkStability hpt sccs all_home_mods = foldl checkSCC ([],[]) sccs
  where
   checkSCC (stable_obj, stable_bco) scc0
     | stableObjects = (scc_mods ++ stable_obj, stable_bco)
     | stableBCOs    = (stable_obj, scc_mods ++ stable_bco)
     | otherwise     = (stable_obj, stable_bco)
     where
	scc = flattenSCC scc0
	scc_mods = map ms_mod_name scc
	home_module m   = m `elem` all_home_mods && m `notElem` scc_mods

        scc_allimps = nub (filter home_module (concatMap ms_home_allimps scc))
	
	stable_obj_imps = map (`elem` stable_obj) scc_allimps
	stable_bco_imps = map (`elem` stable_bco) scc_allimps

	stableObjects = 
	   and stable_obj_imps
	   && all object_ok scc

	stableBCOs = 
	   and (zipWith (||) stable_obj_imps stable_bco_imps)
	   && all bco_ok scc

	object_ok ms
	  | Just t <- ms_obj_date ms  =  t >= ms_hs_date ms 
					 && same_as_prev t
	  | otherwise = False
	  where
	     same_as_prev t = case lookupUFM hpt (ms_mod_name ms) of
				Just hmi  | Just l <- hm_linkable hmi
				 -> isObjectLinkable l && t == linkableTime l
				_other  -> True

	bco_ok ms
	  = case lookupUFM hpt (ms_mod_name ms) of
		Just hmi  | Just l <- hm_linkable hmi ->
			not (isObjectLinkable l) && 
			linkableTime l >= ms_hs_date ms
		_other  -> False

pruneHomePackageTable
   :: HomePackageTable
   -> [ModSummary]
   -> ([ModuleName],[ModuleName])
   -> HomePackageTable

pruneHomePackageTable hpt summ (stable_obj, stable_bco)
  = mapUFM prune hpt
  where prune hmi
	  | is_stable modl = hmi'
	  | otherwise      = hmi'{ hm_details = emptyModDetails }
	  where
	   modl = moduleName (mi_module (hm_iface hmi))
	   hmi' | Just l <- hm_linkable hmi, linkableTime l < ms_hs_date ms
		= hmi{ hm_linkable = Nothing }
		| otherwise
		= hmi
		where ms = expectJust "prune" (lookupUFM ms_map modl)

        ms_map = listToUFM [(ms_mod_name ms, ms) | ms <- summ]

	is_stable m = m `elem` stable_obj || m `elem` stable_bco

findPartiallyCompletedCycles :: [Module] -> [SCC ModSummary] -> [Module]
findPartiallyCompletedCycles modsDone theGraph
   = chew theGraph
     where
        chew [] = []
        chew ((AcyclicSCC _):rest) = chew rest
        chew ((CyclicSCC vs):rest)
           = let names_in_this_cycle = nub (map ms_mod vs)
                 mods_in_this_cycle  
                    = nub ([done | done <- modsDone, 
                                   done `elem` names_in_this_cycle])
                 chewed_rest = chew rest
             in 
             if   notNull mods_in_this_cycle
                  && length mods_in_this_cycle < length names_in_this_cycle
             then mods_in_this_cycle ++ chewed_rest
             else chewed_rest

upsweep
    :: GhcMonad m
    => HomePackageTable
    -> ([ModuleName],[ModuleName])
    -> IO ()
    -> [SCC ModSummary]
    -> m (SuccessFlag,
          [ModSummary])

upsweep old_hpt stable_mods cleanup sccs = do
   (res, done) <- upsweep' old_hpt [] sccs 1 (length sccs)
   return (res, reverse done)
 where

  upsweep' _old_hpt done
     [] _ _
   = return (Succeeded, done)

  upsweep' _old_hpt done
     (CyclicSCC ms:_) _ _
   = do dflags <- getSessionDynFlags
        liftIO $ fatalErrorMsg dflags (cyclicModuleErr ms)
        return (Failed, done)

  upsweep' old_hpt done
     (AcyclicSCC mod:mods) mod_index nmods
   = do
        let logger _mod = defaultWarnErrLogger

        hsc_env <- getSession
        mb_mod_info
            <- handleSourceError
                   (\err -> do logger mod (Just err); return Nothing) $ do
                 mod_info <- liftIO $ upsweep_mod hsc_env old_hpt stable_mods
                                                  mod mod_index nmods
                 logger mod Nothing
                 return (Just mod_info)

        liftIO cleanup

        case mb_mod_info of
          Nothing -> return (Failed, done)
          Just mod_info -> do
		let this_mod = ms_mod_name mod

		    hpt1     = addToUFM (hsc_HPT hsc_env) this_mod mod_info
		    hsc_env1 = hsc_env { hsc_HPT = hpt1 }

		    old_hpt1 | isBootSummary mod = old_hpt
			     | otherwise = delFromUFM old_hpt this_mod

                    done' = mod:done

                hsc_env2 <- liftIO $ reTypecheckLoop hsc_env1 mod done'
                setSession hsc_env2

		upsweep' old_hpt1 done' mods (mod_index+1) nmods

upsweep_mod :: HscEnv
            -> HomePackageTable
	    -> ([ModuleName],[ModuleName])
            -> ModSummary
            -> Int
            -> Int
            -> IO HomeModInfo

upsweep_mod hsc_env old_hpt (stable_obj, stable_bco) summary mod_index nmods
   =    let 
       	    this_mod_name = ms_mod_name summary
	    this_mod    = ms_mod summary
	    mb_obj_date = ms_obj_date summary
	    obj_fn	= ml_obj_file (ms_location summary)
	    hs_date     = ms_hs_date summary

	    is_stable_obj = this_mod_name `elem` stable_obj
	    is_stable_bco = this_mod_name `elem` stable_bco

	    old_hmi = lookupUFM old_hpt this_mod_name

            dflags = ms_hspp_opts summary
            prevailing_target = hscTarget (hsc_dflags hsc_env)
            local_target      = hscTarget dflags

            target = if prevailing_target /= local_target
                        && (not (isObjectTarget prevailing_target)
                            || not (isObjectTarget local_target))
                        then prevailing_target
                        else local_target 

            summary' = summary{ ms_hspp_opts = dflags { hscTarget = target } }

            mb_old_iface 
	    	= case old_hmi of
	    	     Nothing	 			  -> Nothing
	    	     Just hm_info | isBootSummary summary -> Just iface
	    			  | not (mi_boot iface)   -> Just iface
	    			  | otherwise		  -> Nothing
	    			   where 
	    			     iface = hm_iface hm_info

	    compile_it :: Maybe Linkable -> IO HomeModInfo
	    compile_it  mb_linkable = 
                  compile hsc_env summary' mod_index nmods 
                          mb_old_iface mb_linkable

            compile_it_discard_iface :: Maybe Linkable -> IO HomeModInfo
            compile_it_discard_iface mb_linkable =
                  compile hsc_env summary' mod_index nmods
                          Nothing mb_linkable

            is_fake_linkable
               | Just hmi <- old_hmi, Just l <- hm_linkable hmi =
                  null (linkableUnlinked l)
               | otherwise =
                   False

            implies False _ = True
            implies True x  = x

        in
        case () of
         _
          | is_stable_obj, Just hmi <- old_hmi -> do
                liftIO $ debugTraceMsg (hsc_dflags hsc_env) 5
                           (text "skipping stable obj mod:" <+> ppr this_mod_name)
                return hmi

          | is_stable_obj, isNothing old_hmi -> do
                liftIO $ debugTraceMsg (hsc_dflags hsc_env) 5
                           (text "compiling stable on-disk mod:" <+> ppr this_mod_name)
                linkable <- liftIO $ findObjectLinkable this_mod obj_fn
                              (expectJust "upsweep1" mb_obj_date)
                compile_it (Just linkable)

          | not (isObjectTarget target), is_stable_bco,
            (target /= HscNothing) `implies` not is_fake_linkable ->
                ASSERT(isJust old_hmi)
                let Just hmi = old_hmi in do
                liftIO $ debugTraceMsg (hsc_dflags hsc_env) 5
                           (text "skipping stable BCO mod:" <+> ppr this_mod_name)
                return hmi

          | not (isObjectTarget target),
            Just hmi <- old_hmi,
            Just l <- hm_linkable hmi,
            not (isObjectLinkable l),
            (target /= HscNothing) `implies` not is_fake_linkable,
            linkableTime l >= ms_hs_date summary -> do
                liftIO $ debugTraceMsg (hsc_dflags hsc_env) 5
                           (text "compiling non-stable BCO mod:" <+> ppr this_mod_name)
                compile_it (Just l)

          | isObjectTarget target,
            Just obj_date <- mb_obj_date,
            obj_date >= hs_date -> do
                case old_hmi of
                  Just hmi
                    | Just l <- hm_linkable hmi,
                      isObjectLinkable l && linkableTime l == obj_date -> do
                          liftIO $ debugTraceMsg (hsc_dflags hsc_env) 5
                                     (text "compiling mod with new on-disk obj:" <+> ppr this_mod_name)
                          compile_it (Just l)
                  _otherwise -> do
                          liftIO $ debugTraceMsg (hsc_dflags hsc_env) 5
                                     (text "compiling mod with new on-disk obj2:" <+> ppr this_mod_name)
                          linkable <- liftIO $ findObjectLinkable this_mod obj_fn obj_date
                          compile_it_discard_iface (Just linkable)

         _otherwise -> do
                liftIO $ debugTraceMsg (hsc_dflags hsc_env) 5
                           (text "compiling mod:" <+> ppr this_mod_name)
                compile_it Nothing

retainInTopLevelEnvs :: [ModuleName] -> HomePackageTable -> HomePackageTable
retainInTopLevelEnvs keep_these hpt
   = listToUFM   [ (mod, expectJust "retain" mb_mod_info)
		 | mod <- keep_these
		 , let mb_mod_info = lookupUFM hpt mod
		 , isJust mb_mod_info ]

reTypecheckLoop :: HscEnv -> ModSummary -> ModuleGraph -> IO HscEnv
reTypecheckLoop hsc_env ms graph
  | not (isBootSummary ms) && 
    any (\m -> ms_mod m == this_mod && isBootSummary m) graph
  = do
        let mss = reachableBackwards (ms_mod_name ms) graph
            non_boot = filter (not.isBootSummary) mss
        debugTraceMsg (hsc_dflags hsc_env) 2 $
           text "Re-typechecking loop: " <> ppr (map ms_mod_name non_boot)
        typecheckLoop hsc_env (map ms_mod_name non_boot)
  | otherwise
  = return hsc_env
 where
  this_mod = ms_mod ms

typecheckLoop :: HscEnv -> [ModuleName] -> IO HscEnv
typecheckLoop hsc_env mods = do
  new_hpt <-
    fixIO $ \new_hpt -> do
      let new_hsc_env = hsc_env{ hsc_HPT = new_hpt }
      mds <- initIfaceCheck new_hsc_env $ 
                mapM (typecheckIface . hm_iface) hmis
      let new_hpt = addListToUFM old_hpt 
                        (zip mods [ hmi{ hm_details = details }
                                  | (hmi,details) <- zip hmis mds ])
      return new_hpt
  return hsc_env{ hsc_HPT = new_hpt }
  where
    old_hpt = hsc_HPT hsc_env
    hmis    = map (expectJust "typecheckLoop" . lookupUFM old_hpt) mods

reachableBackwards :: ModuleName -> [ModSummary] -> [ModSummary]
reachableBackwards mod summaries
  = [ ms | (ms,_,_) <- reachableG (transposeG graph) root ]
  where
        (graph, lookup_node) = moduleGraphNodes False summaries
        root  = expectJust "reachableBackwards" (lookup_node HsBootFile mod)

type SummaryNode = (ModSummary, Int, [Int])

topSortModuleGraph
	  :: Bool
	  -> [ModSummary]
	  -> Maybe ModuleName
	  -> [SCC ModSummary]

topSortModuleGraph drop_hs_boot_nodes summaries mb_root_mod
  = map (fmap summaryNodeSummary) $ stronglyConnCompG initial_graph
  where
    (graph, lookup_node) = moduleGraphNodes drop_hs_boot_nodes summaries
    
    initial_graph = case mb_root_mod of
        Nothing -> graph
        Just root_mod ->
            let root | Just node <- lookup_node HsSrcFile root_mod, graph `hasVertexG` node = node
                     | otherwise = ghcError (ProgramError "module does not exist")
            in graphFromEdgedVertices (seq root (reachableG graph root))

summaryNodeKey :: SummaryNode -> Int
summaryNodeKey (_, k, _) = k

summaryNodeSummary :: SummaryNode -> ModSummary
summaryNodeSummary (s, _, _) = s

moduleGraphNodes :: Bool -> [ModSummary]
  -> (Graph SummaryNode, HscSource -> ModuleName -> Maybe SummaryNode)
moduleGraphNodes drop_hs_boot_nodes summaries = (graphFromEdgedVertices nodes, lookup_node)
  where
    numbered_summaries = zip summaries [1..]

    lookup_node :: HscSource -> ModuleName -> Maybe SummaryNode
    lookup_node hs_src mod = Map.lookup (mod, hs_src) node_map

    lookup_key :: HscSource -> ModuleName -> Maybe Int
    lookup_key hs_src mod = fmap summaryNodeKey (lookup_node hs_src mod)

    node_map :: NodeMap SummaryNode
    node_map = Map.fromList [ ((moduleName (ms_mod s), ms_hsc_src s), node)
                            | node@(s, _, _) <- nodes ]

    nodes :: [SummaryNode]
    nodes = [ (s, key, out_keys)
            | (s, key) <- numbered_summaries
            , not (isBootSummary s && drop_hs_boot_nodes)
            , let out_keys = out_edge_keys hs_boot_key (map unLoc (ms_home_srcimps s)) ++
                             out_edge_keys HsSrcFile   (map unLoc (ms_home_imps s)) ++
                             (
                              if drop_hs_boot_nodes || ms_hsc_src s == HsBootFile 
                              then [] 
                              else case lookup_key HsBootFile (ms_mod_name s) of
                                    Nothing -> []
                                    Just k  -> [k]) ]

    hs_boot_key | drop_hs_boot_nodes = HsSrcFile
                | otherwise          = HsBootFile

    out_edge_keys :: HscSource -> [ModuleName] -> [Int]
    out_edge_keys hi_boot ms = mapCatMaybes (lookup_key hi_boot) ms

type NodeKey   = (ModuleName, HscSource)
type NodeMap a = Map NodeKey a

msKey :: ModSummary -> NodeKey
msKey (ModSummary { ms_mod = mod, ms_hsc_src = boot }) = (moduleName mod,boot)

mkNodeMap :: [ModSummary] -> NodeMap ModSummary
mkNodeMap summaries = Map.fromList [ (msKey s, s) | s <- summaries]
	
nodeMapElts :: NodeMap a -> [a]
nodeMapElts = Map.elems

warnUnnecessarySourceImports :: GhcMonad m => [SCC ModSummary] -> m ()
warnUnnecessarySourceImports sccs = do
  logWarnings (listToBag (concatMap (check.flattenSCC) sccs))
  where check ms =
	   let mods_in_this_cycle = map ms_mod_name ms in
	   [ warn i | m <- ms, i <- ms_home_srcimps m,
	              unLoc i `notElem`  mods_in_this_cycle ]

	warn :: Located ModuleName -> WarnMsg
	warn (L loc mod) = 
	   mkPlainErrMsg loc
		(ptext (sLit "Warning: {-# SOURCE #-} unnecessary in import of ")
		 <+> quotes (ppr mod))

downsweep :: HscEnv
	  -> [ModSummary]
	  -> [ModuleName]
	  -> Bool
	  -> IO [ModSummary]
downsweep hsc_env old_summaries excl_mods allow_dup_roots
   = do
       rootSummaries <- mapM getRootSummary roots
       let root_map = mkRootMap rootSummaries
       checkDuplicates root_map
       summs <- loop (concatMap msDeps rootSummaries) root_map
       return summs
     where
	roots = hsc_targets hsc_env

	old_summary_map :: NodeMap ModSummary
	old_summary_map = mkNodeMap old_summaries

	getRootSummary :: Target -> IO ModSummary
	getRootSummary (Target (TargetFile file mb_phase) obj_allowed maybe_buf)
	   = do exists <- liftIO $ doesFileExist file
		if exists 
		    then summariseFile hsc_env old_summaries file mb_phase 
                                       obj_allowed maybe_buf
		    else throwOneError $ mkPlainErrMsg noSrcSpan $
			   text "can't find file:" <+> text file
	getRootSummary (Target (TargetModule modl) obj_allowed maybe_buf)
 	   = do maybe_summary <- summariseModule hsc_env old_summary_map False 
					   (L rootLoc modl) obj_allowed 
                                           maybe_buf excl_mods
		case maybe_summary of
		   Nothing -> packageModErr modl
		   Just s  -> return s

	rootLoc = mkGeneralSrcSpan (fsLit "<command line>")

	checkDuplicates :: NodeMap [ModSummary] -> IO ()
	checkDuplicates root_map 
	   | allow_dup_roots = return ()
	   | null dup_roots  = return ()
	   | otherwise	     = liftIO $ multiRootsErr (head dup_roots)
	   where
	     dup_roots :: [[ModSummary]]
	     dup_roots = filterOut isSingleton (nodeMapElts root_map)

	loop :: [(Located ModuleName,IsBootInterface)]
	     -> NodeMap [ModSummary]
	     -> IO [ModSummary]
	loop [] done 	  = return (concat (nodeMapElts done))
	loop ((wanted_mod, is_boot) : ss) done 
	  | Just summs <- Map.lookup key done
	  = if isSingleton summs then
		loop ss done
	    else
		do { multiRootsErr summs; return [] }
	  | otherwise
          = do mb_s <- summariseModule hsc_env old_summary_map 
                                       is_boot wanted_mod True
                                       Nothing excl_mods
               case mb_s of
                   Nothing -> loop ss done
                   Just s  -> loop (msDeps s ++ ss) (Map.insert key [s] done)
	  where
	    key = (unLoc wanted_mod, if is_boot then HsBootFile else HsSrcFile)

mkRootMap :: [ModSummary] -> NodeMap [ModSummary]
mkRootMap summaries = Map.insertListWith (flip (++))
                                         [ (msKey s, [s]) | s <- summaries ]
                                         Map.empty

msDeps :: ModSummary -> [(Located ModuleName, IsBootInterface)]
msDeps s = 
    concat [ [(m,True), (m,False)] | m <- ms_home_srcimps s ] 
	 ++ [ (m,False) | m <- ms_home_imps s ] 

home_imps :: [Located (ImportDecl RdrName)] -> [Located ModuleName]
home_imps imps = [ ideclName i |  L _ i <- imps, isLocal (ideclPkgQual i) ]
  where isLocal Nothing = True
        isLocal (Just pkg) | pkg == fsLit "this" = True -- "this" is special
        isLocal _ = False

ms_home_allimps :: ModSummary -> [ModuleName]
ms_home_allimps ms = map unLoc (ms_home_srcimps ms ++ ms_home_imps ms)

ms_home_srcimps :: ModSummary -> [Located ModuleName]
ms_home_srcimps = home_imps . ms_srcimps

ms_home_imps :: ModSummary -> [Located ModuleName]
ms_home_imps = home_imps . ms_imps

summariseFile
	:: HscEnv
	-> [ModSummary]
	-> FilePath
	-> Maybe Phase
        -> Bool
	-> Maybe (StringBuffer,ClockTime)
	-> IO ModSummary

summariseFile hsc_env old_summaries file mb_phase obj_allowed maybe_buf
   | Just old_summary <- findSummaryBySourceFile old_summaries file
   = do
	let location = ms_location old_summary

	src_timestamp <- case maybe_buf of
			   Just (_,t) -> return t
			   Nothing    -> liftIO $ getModificationTime file

	if ms_hs_date old_summary == src_timestamp 
	   then do
        	  obj_timestamp <-
                    if isObjectTarget (hscTarget (hsc_dflags hsc_env)) 
                        || obj_allowed
                        then liftIO $ getObjTimestamp location False
                        else return Nothing
		  return old_summary{ ms_obj_date = obj_timestamp }
	   else
		new_summary

   | otherwise
   = new_summary
  where
    new_summary = do
   	let dflags = hsc_dflags hsc_env

	(dflags', hspp_fn, buf)
	    <- preprocessFile hsc_env file mb_phase maybe_buf

        (srcimps,the_imps, L _ mod_name) <- getImports dflags' buf hspp_fn file

	location <- liftIO $ mkHomeModLocation dflags mod_name file

	mod <- liftIO $ addHomeModuleToFinder hsc_env mod_name location

        src_timestamp <- case maybe_buf of
			   Just (_,t) -> return t
			   Nothing    -> liftIO $ getModificationTime file

	obj_timestamp <-
            if isObjectTarget (hscTarget (hsc_dflags hsc_env)) 
               || obj_allowed
                then liftIO $ modificationTimeIfExists (ml_obj_file location)
                else return Nothing

        return (ModSummary { ms_mod = mod, ms_hsc_src = HsSrcFile,
			     ms_location = location,
                             ms_hspp_file = hspp_fn,
                             ms_hspp_opts = dflags',
			     ms_hspp_buf  = Just buf,
                             ms_srcimps = srcimps, ms_imps = the_imps,
			     ms_hs_date = src_timestamp,
			     ms_obj_date = obj_timestamp })

findSummaryBySourceFile :: [ModSummary] -> FilePath -> Maybe ModSummary
findSummaryBySourceFile summaries file
  = case [ ms | ms <- summaries, HsSrcFile <- [ms_hsc_src ms],
			         expectJust "findSummaryBySourceFile" (ml_hs_file (ms_location ms)) == file ] of
	[] -> Nothing
	(x:_) -> Just x

summariseModule
	  :: HscEnv
	  -> NodeMap ModSummary
	  -> IsBootInterface
	  -> Located ModuleName
          -> Bool
	  -> Maybe (StringBuffer, ClockTime)
	  -> [ModuleName]
	  -> IO (Maybe ModSummary)

summariseModule hsc_env old_summary_map is_boot (L loc wanted_mod) 
                obj_allowed maybe_buf excl_mods
  | wanted_mod `elem` excl_mods
  = return Nothing

  | Just old_summary <- Map.lookup (wanted_mod, hsc_src) old_summary_map
  = do
	let location = ms_location old_summary
	    src_fn = expectJust "summariseModule" (ml_hs_file location)

	case maybe_buf of
	   Just (_,t) -> check_timestamp old_summary location src_fn t
	   Nothing    -> do
		m <- tryIO (getModificationTime src_fn)
		case m of
		   Right t -> check_timestamp old_summary location src_fn t
		   Left e | isDoesNotExistError e -> find_it
		          | otherwise             -> ioError e

  | otherwise  = find_it
  where
    dflags = hsc_dflags hsc_env

    hsc_src = if is_boot then HsBootFile else HsSrcFile

    check_timestamp old_summary location src_fn src_timestamp
	| ms_hs_date old_summary == src_timestamp = do
                obj_timestamp <- 
                    if isObjectTarget (hscTarget (hsc_dflags hsc_env))
                       || obj_allowed
                       then getObjTimestamp location is_boot
                       else return Nothing
		return (Just old_summary{ ms_obj_date = obj_timestamp })
	| otherwise = 
		new_summary location (ms_mod old_summary) src_fn src_timestamp

    find_it = do
	uncacheModule hsc_env wanted_mod
	found <- findImportedModule hsc_env wanted_mod Nothing
	case found of
	     Found location mod 
		| isJust (ml_hs_file location) ->
			 just_found location mod
		| otherwise -> 
			ASSERT(modulePackageId mod /= thisPackage dflags)
			return Nothing
			
	     err -> noModError dflags loc wanted_mod err

    just_found location mod = do
	let location' | is_boot   = addBootSuffixLocn location
		      | otherwise = location
	    src_fn = expectJust "summarise2" (ml_hs_file location')

	maybe_t <- modificationTimeIfExists src_fn
	case maybe_t of
	  Nothing -> noHsFileErr loc src_fn
	  Just t  -> new_summary location' mod src_fn t

    new_summary location mod src_fn src_timestamp
      = do
	(dflags', hspp_fn, buf) <- preprocessFile hsc_env src_fn Nothing maybe_buf
        (srcimps, the_imps, L mod_loc mod_name) <- getImports dflags' buf hspp_fn src_fn

	when (mod_name /= wanted_mod) $
		throwOneError $ mkPlainErrMsg mod_loc $ 
			      text "File name does not match module name:" 
			      $$ text "Saw:" <+> quotes (ppr mod_name)
                              $$ text "Expected:" <+> quotes (ppr wanted_mod)

	obj_timestamp <-
           if isObjectTarget (hscTarget (hsc_dflags hsc_env))
              || obj_allowed
              then getObjTimestamp location is_boot
              else return Nothing

	return (Just (ModSummary { ms_mod       = mod,
			      ms_hsc_src   = hsc_src,
			      ms_location  = location,
			      ms_hspp_file = hspp_fn,
                              ms_hspp_opts = dflags',
			      ms_hspp_buf  = Just buf,
			      ms_srcimps   = srcimps,
			      ms_imps      = the_imps,
			      ms_hs_date   = src_timestamp,
			      ms_obj_date  = obj_timestamp }))

getObjTimestamp :: ModLocation -> Bool -> IO (Maybe ClockTime)
getObjTimestamp location is_boot
  = if is_boot then return Nothing
	       else modificationTimeIfExists (ml_obj_file location)

preprocessFile :: HscEnv
               -> FilePath
               -> Maybe Phase
               -> Maybe (StringBuffer,ClockTime)
               -> IO (DynFlags, FilePath, StringBuffer)
preprocessFile hsc_env src_fn mb_phase Nothing
  = do
	(dflags', hspp_fn) <- preprocess hsc_env (src_fn, mb_phase)
	buf <- hGetStringBuffer hspp_fn
	return (dflags', hspp_fn, buf)

preprocessFile hsc_env src_fn mb_phase (Just (buf, _time))
  = do
        let dflags = hsc_dflags hsc_env
	let 
	    local_opts = getOptions dflags buf src_fn
	(dflags', leftovers, warns)
            <- parseDynamicNoPackageFlags dflags local_opts
        checkProcessArgsResult leftovers
        handleFlagWarnings dflags' warns

	let
	    needs_preprocessing
		| Just (Unlit _) <- mb_phase    = True
	        | Nothing <- mb_phase, Unlit _ <- startPhase src_fn  = True
		| xopt Opt_Cpp dflags'		= True
		| dopt Opt_Pp  dflags'		= True
		| otherwise			= False

	when needs_preprocessing $
	   ghcError (ProgramError "buffer needs preprocesing; interactive check disabled")

	return (dflags', src_fn, buf)

noModError :: DynFlags -> SrcSpan -> ModuleName -> FindResult -> IO ab
noModError dflags loc wanted_mod err
  = throwOneError $ mkPlainErrMsg loc $ cannotFindModule dflags wanted_mod err
				
noHsFileErr :: SrcSpan -> String -> IO a
noHsFileErr loc path
  = throwOneError $ mkPlainErrMsg loc $ text "Can't find" <+> text path
 
packageModErr :: ModuleName -> IO a
packageModErr mod
  = throwOneError $ mkPlainErrMsg noSrcSpan $
	text "module" <+> quotes (ppr mod) <+> text "is a package module"

multiRootsErr :: [ModSummary] -> IO ()
multiRootsErr [] = panic "multiRootsErr"
multiRootsErr summs@(summ1:_)
  = throwOneError $ mkPlainErrMsg noSrcSpan $
	text "module" <+> quotes (ppr mod) <+> 
	text "is defined in multiple files:" <+>
	sep (map text files)
  where
    mod = ms_mod summ1
    files = map (expectJust "checkDup" . ml_hs_file . ms_location) summs

cyclicModuleErr :: [ModSummary] -> SDoc
cyclicModuleErr ms
  = hang (ptext (sLit "Module imports form a cycle for modules:"))
       2 (vcat (map show_one ms))
  where
    mods_in_cycle = map ms_mod_name ms
    imp_modname = unLoc . ideclName . unLoc
    just_in_cycle = filter ((`elem` mods_in_cycle) . imp_modname)

    show_one ms = 
           vcat [ show_mod (ms_hsc_src ms) (ms_mod_name ms) <+>
                  maybe empty (parens . text) (ml_hs_file (ms_location ms)),
                  nest 2 $ ptext (sLit "imports:") <+> vcat [
                     pp_imps HsBootFile (just_in_cycle $ ms_srcimps ms),
                     pp_imps HsSrcFile  (just_in_cycle $ ms_imps ms) ]
                ]
    show_mod hsc_src mod = ppr mod <> text (hscSourceString hsc_src)
    pp_imps src imps = fsep (map (show_mod src . unLoc . ideclName . unLoc) imps)

workingDirectoryChanged :: GhcMonad m => m ()
workingDirectoryChanged = withSession $ (liftIO . flushFinderCaches)

getModuleGraph :: GhcMonad m => m ModuleGraph
getModuleGraph = liftM hsc_mod_graph getSession

needsTemplateHaskell :: ModuleGraph -> Bool
needsTemplateHaskell ms =
    any (xopt Opt_TemplateHaskell . ms_hspp_opts) ms

isLoaded :: GhcMonad m => ModuleName -> m Bool
isLoaded m = withSession $ \hsc_env ->
  return $! isJust (lookupUFM (hsc_HPT hsc_env) m)

getBindings :: GhcMonad m => m [TyThing]
getBindings = withSession $ \hsc_env ->
   let 
       occ_env = mkOccEnv [ (nameOccName (idName id), AnId id) 
                          | id <- ic_tmp_ids (hsc_IC hsc_env) ]
   in
   return (occEnvElts occ_env)

getPrintUnqual :: GhcMonad m => m PrintUnqualified
getPrintUnqual = withSession $ \hsc_env ->
  return (icPrintUnqual (hsc_dflags hsc_env) (hsc_IC hsc_env))

data ModuleInfo = ModuleInfo {
	minf_type_env  :: TypeEnv,
	minf_exports   :: NameSet,
	minf_rdr_env   :: Maybe GlobalRdrEnv,
	minf_instances :: [Instance]
#ifdef GHCI
        ,minf_modBreaks :: ModBreaks 
#endif
  }

getModuleInfo :: GhcMonad m => Module -> m (Maybe ModuleInfo)
getModuleInfo mdl = withSession $ \hsc_env -> do
  let mg = hsc_mod_graph hsc_env
  if mdl `elem` map ms_mod mg
	then liftIO $ getHomeModuleInfo hsc_env (moduleName mdl)
	else do
  
 liftIO $ getPackageModuleInfo hsc_env mdl

getPackageModuleInfo :: HscEnv -> Module -> IO (Maybe ModuleInfo)
#ifdef GHCI
getPackageModuleInfo hsc_env mdl = do
  mb_avails <- hscGetModuleExports hsc_env mdl
  case mb_avails of
    Nothing -> return Nothing
    Just avails -> do
	eps <- readIORef (hsc_EPS hsc_env)
	let 
            names  = availsToNameSet avails
	    pte    = eps_PTE eps
	    tys    = [ ty | name <- concatMap availNames avails,
			    Just ty <- [lookupTypeEnv pte name] ]
	return (Just (ModuleInfo {
			minf_type_env  = mkTypeEnv tys,
			minf_exports   = names,
			minf_rdr_env   = Just $! availsToGlobalRdrEnv (moduleName mdl) avails,
			minf_instances = error "getModuleInfo: instances for package module unimplemented",
                        minf_modBreaks = emptyModBreaks  
		}))
#else
getPackageModuleInfo _hsc_env _mdl = do
  return Nothing
#endif

getHomeModuleInfo :: HscEnv -> ModuleName -> IO (Maybe ModuleInfo)
getHomeModuleInfo hsc_env mdl = 
  case lookupUFM (hsc_HPT hsc_env) mdl of
    Nothing  -> return Nothing
    Just hmi -> do
      let details = hm_details hmi
      return (Just (ModuleInfo {
			minf_type_env  = md_types details,
			minf_exports   = availsToNameSet (md_exports details),
			minf_rdr_env   = mi_globals $! hm_iface hmi,
			minf_instances = md_insts details
#ifdef GHCI
                       ,minf_modBreaks = getModBreaks hmi
#endif
			}))

modInfoTyThings :: ModuleInfo -> [TyThing]
modInfoTyThings minf = typeEnvElts (minf_type_env minf)

modInfoTopLevelScope :: ModuleInfo -> Maybe [Name]
modInfoTopLevelScope minf
  = fmap (map gre_name . globalRdrEnvElts) (minf_rdr_env minf)

modInfoExports :: ModuleInfo -> [Name]
modInfoExports minf = nameSetToList $! minf_exports minf

modInfoInstances :: ModuleInfo -> [Instance]
modInfoInstances = minf_instances

modInfoIsExportedName :: ModuleInfo -> Name -> Bool
modInfoIsExportedName minf name = elemNameSet name (minf_exports minf)

mkPrintUnqualifiedForModule :: GhcMonad m =>
                               ModuleInfo
                            -> m (Maybe PrintUnqualified)
mkPrintUnqualifiedForModule minf = withSession $ \hsc_env -> do
  return (fmap (mkPrintUnqualified (hsc_dflags hsc_env)) (minf_rdr_env minf))

modInfoLookupName :: GhcMonad m =>
                     ModuleInfo -> Name
                  -> m (Maybe TyThing)
modInfoLookupName minf name = withSession $ \hsc_env -> do
   case lookupTypeEnv (minf_type_env minf) name of
     Just tyThing -> return (Just tyThing)
     Nothing      -> do
       eps <- liftIO $ readIORef (hsc_EPS hsc_env)
       return $! lookupType (hsc_dflags hsc_env) 
			    (hsc_HPT hsc_env) (eps_PTE eps) name

#ifdef GHCI
modInfoModBreaks :: ModuleInfo -> ModBreaks
modInfoModBreaks = minf_modBreaks  
#endif

isDictonaryId :: Id -> Bool
isDictonaryId id
  = case tcSplitSigmaTy (idType id) of { (_tvs, _theta, tau) -> isDictTy tau }

lookupGlobalName :: GhcMonad m => Name -> m (Maybe TyThing)
lookupGlobalName name = withSession $ \hsc_env -> do
   liftIO $ lookupTypeHscEnv hsc_env name

findGlobalAnns :: (GhcMonad m, Typeable a) => ([Word8] -> a) -> AnnTarget Name -> m [a]
findGlobalAnns deserialize target = withSession $ \hsc_env -> do
    ann_env <- liftIO $ prepareAnnotations hsc_env Nothing
    return (findAnns deserialize ann_env target)

#ifdef GHCI
getGRE :: GhcMonad m => m GlobalRdrEnv
getGRE = withSession $ \hsc_env-> return $ ic_rn_gbl_env (hsc_IC hsc_env)
#endif

packageDbModules :: GhcMonad m =>
                    Bool
                 -> m [Module]
packageDbModules only_exposed = do
   dflags <- getSessionDynFlags
   let pkgs = eltsUFM (pkgIdMap (pkgState dflags))
   return $
     [ mkModule pid modname | p <- pkgs
                            , not only_exposed || exposed p
                            , let pid = packageConfigId p
                            , modname <- exposedModules p ]

dataConType :: DataCon -> Type
dataConType dc = idType (dataConWrapId dc)

pprParenSymName :: NamedThing a => a -> SDoc
pprParenSymName a = parenSymOcc (getOccName a) (ppr (getName a))

#if 0

#endif

getModuleSourceAndFlags :: GhcMonad m => Module -> m (String, StringBuffer, DynFlags)
getModuleSourceAndFlags mod = do
  m <- getModSummary (moduleName mod)
  case ml_hs_file $ ms_location m of
    Nothing -> throw $ mkApiErr (text "No source available for module " <+> ppr mod)
    Just sourceFile -> do
        source <- liftIO $ hGetStringBuffer sourceFile
        return (sourceFile, source, ms_hspp_opts m)

getTokenStream :: GhcMonad m => Module -> m [Located Token]
getTokenStream mod = do
  (sourceFile, source, flags) <- getModuleSourceAndFlags mod
  let startLoc = mkSrcLoc (mkFastString sourceFile) 1 1
  case lexTokenStream source startLoc flags of
    POk _ ts  -> return ts
    PFailed span err -> throw $ mkSrcErr (unitBag $ mkPlainErrMsg span err)

getRichTokenStream :: GhcMonad m => Module -> m [(Located Token, String)]
getRichTokenStream mod = do
  (sourceFile, source, flags) <- getModuleSourceAndFlags mod
  let startLoc = mkSrcLoc (mkFastString sourceFile) 1 1
  case lexTokenStream source startLoc flags of
    POk _ ts -> return $ addSourceToTokens startLoc source ts
    PFailed span err -> throw $ mkSrcErr (unitBag $ mkPlainErrMsg span err)

addSourceToTokens :: SrcLoc -> StringBuffer -> [Located Token]
                  -> [(Located Token, String)]
addSourceToTokens _ _ [] = []
addSourceToTokens loc buf (t@(L span _) : ts)
    | not (isGoodSrcSpan span) = (t,"") : addSourceToTokens loc buf ts
    | otherwise = (t,str) : addSourceToTokens newLoc newBuf ts
    where
      (newLoc, newBuf, str) = go "" loc buf
      start = srcSpanStart span
      end = srcSpanEnd span
      go acc loc buf | loc < start = go acc nLoc nBuf
                     | start <= loc && loc < end = go (ch:acc) nLoc nBuf
                     | otherwise = (loc, buf, reverse acc)
          where (ch, nBuf) = nextChar buf
                nLoc = advanceSrcLoc loc ch

showRichTokenStream :: [(Located Token, String)] -> String
showRichTokenStream ts = go startLoc ts ""
    where sourceFile = srcSpanFile (getLoc . fst . head $ ts)
          startLoc = mkSrcLoc sourceFile 1 1
          go _ [] = id
          go loc ((L span _, str):ts)
              | not (isGoodSrcSpan span) = go loc ts
              | locLine == tokLine = ((replicate (tokCol - locCol) ' ') ++)
                                     . (str ++)
                                     . go tokEnd ts
              | otherwise = ((replicate (tokLine - locLine) '\n') ++)
                            . ((replicate tokCol ' ') ++)
                            . (str ++)
                            . go tokEnd ts
              where (locLine, locCol) = (srcLocLine loc, srcLocCol loc)
                    (tokLine, tokCol) = (srcSpanStartLine span, srcSpanStartCol span)
                    tokEnd = srcSpanEnd span

findModule :: GhcMonad m => ModuleName -> Maybe FastString -> m Module
findModule mod_name maybe_pkg = withSession $ \hsc_env -> do
  let 
    dflags   = hsc_dflags hsc_env
    this_pkg = thisPackage dflags
  case maybe_pkg of
    Just pkg | fsToPackageId pkg /= this_pkg && pkg /= fsLit "this" -> liftIO $ do
      res <- findImportedModule hsc_env mod_name maybe_pkg
      case res of
        Found _ m -> return m
        err       -> noModError dflags noSrcSpan mod_name err
    _otherwise -> do
      home <- lookupLoadedHomeModule mod_name
      case home of
        Just m  -> return m
        Nothing -> liftIO $ do
           res <- findImportedModule hsc_env mod_name maybe_pkg
           case res of
             Found loc m | modulePackageId m /= this_pkg -> return m
                         | otherwise -> modNotLoadedError m loc
             err -> noModError dflags noSrcSpan mod_name err

modNotLoadedError :: Module -> ModLocation -> IO a
modNotLoadedError m loc = ghcError $ CmdLineError $ showSDoc $
   text "module is not loaded:" <+> 
   quotes (ppr (moduleName m)) <+>
   parens (text (expectJust "modNotLoadedError" (ml_hs_file loc)))

lookupModule :: GhcMonad m => ModuleName -> Maybe FastString -> m Module
lookupModule mod_name (Just pkg) = findModule mod_name (Just pkg)
lookupModule mod_name Nothing = withSession $ \hsc_env -> do
  home <- lookupLoadedHomeModule mod_name
  case home of
    Just m  -> return m
    Nothing -> liftIO $ do
      res <- findExposedPackageModule hsc_env mod_name Nothing
      case res of
        Found _ m -> return m
	err       -> noModError (hsc_dflags hsc_env) noSrcSpan mod_name err

lookupLoadedHomeModule  :: GhcMonad m => ModuleName -> m (Maybe Module)
lookupLoadedHomeModule mod_name = withSession $ \hsc_env ->
  case lookupUFM (hsc_HPT hsc_env) mod_name of
    Just mod_info      -> return (Just (mi_module (hm_iface mod_info)))
    _not_a_home_module -> return Nothing

#ifdef GHCI
getHistorySpan :: GhcMonad m => History -> m SrcSpan
getHistorySpan h = withSession $ \hsc_env ->
                          return$ InteractiveEval.getHistorySpan hsc_env h

obtainTermFromVal :: GhcMonad m => Int ->  Bool -> Type -> a -> m Term
obtainTermFromVal bound force ty a =
    withSession $ \hsc_env ->
      liftIO $ InteractiveEval.obtainTermFromVal hsc_env bound force ty a

obtainTermFromId :: GhcMonad m => Int -> Bool -> Id -> m Term
obtainTermFromId bound force id =
    withSession $ \hsc_env ->
      liftIO $ InteractiveEval.obtainTermFromId hsc_env bound force id

#endif

lookupName :: GhcMonad m => Name -> m (Maybe TyThing)
lookupName name =
     withSession $ \hsc_env -> 
       liftIO $ hscTcRcLookupName hsc_env name

parser :: String
       -> DynFlags
       -> FilePath
       -> Either ErrorMessages (WarningMessages, Located (HsModule RdrName))

parser str dflags filename = 
   let
       loc  = mkSrcLoc (mkFastString filename) 1 1
       buf  = stringToStringBuffer str
   in
   case unP Parser.parseModule (mkPState dflags buf loc) of

     PFailed span err   -> 
         Left (unitBag (mkPlainErrMsg span err))

     POk pst rdr_module ->
         let (warns,_) = getMessages pst in
         Right (warns, rdr_module)
