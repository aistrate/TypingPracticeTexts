{-# OPTIONS -fno-cse #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module InteractiveUI ( interactiveUI, ghciWelcomeMsg ) where

#include "HsVersions.h"

import qualified GhciMonad
import GhciMonad hiding (runStmt)
import GhciTags
import Debugger

import qualified GHC hiding (resume, runStmt)
import GHC              ( LoadHowMuch(..), Target(..),  TargetId(..),
                          TyThing(..), Phase,
                          BreakIndex, Resume, SingleStep,
                          Ghc, handleSourceError )
import PprTyThing
import DynFlags
import qualified Lexer
import StringBuffer

import Packages
import UniqFM

import HscTypes ( handleFlagWarnings )
import HsImpExp
import qualified RdrName ( getGRE_NameQualifier_maybes )
import RdrName (RdrName)
import Outputable       hiding (printForUser, printForUserPartWay)
import Module
import Name
import SrcLoc

import Digraph
import BasicTypes hiding (isTopLevel)
import Panic      hiding (showException)
import Config
import StaticFlags
import Linker
import Util
import NameSet
import Maybes		( orElse, expectJust )
import FastString
import Encoding
import Foreign.C

#ifndef mingw32_HOST_OS
import System.Posix hiding (getEnv)
#else
import qualified System.Win32
#endif

import System.Console.Haskeline as Haskeline
import qualified System.Console.Haskeline.Encoding as Encoding
import Control.Monad.Trans

import Exception hiding (catch, block, unblock)

import System.FilePath
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe
import System.Cmd
import System.Environment
import System.Exit	( exitWith, ExitCode(..) )
import System.Directory
import System.IO
import System.IO.Error
import Data.Char
import Data.Array
import Control.Monad as Monad
import Text.Printf
import Foreign
import GHC.Exts		( unsafeCoerce# )

import GHC.IO.Exception	( IOErrorType(InvalidArgument) )
import GHC.IO.Handle    ( hFlushAll )

import GHC.TopHandler

import Data.IORef	( IORef, readIORef, writeIORef )

ghciWelcomeMsg :: String
ghciWelcomeMsg = "GHCi, version " ++ cProjectVersion ++
                 ": http://www.haskell.org/ghc/  :? for help"

cmdName :: Command -> String
cmdName (n,_,_) = n

GLOBAL_VAR(macros_ref, [], [Command])

builtin_commands :: [Command]
builtin_commands = [
  ("?",         keepGoing help,                 noCompletion),
  ("add",       keepGoingPaths addModule,       completeFilename),
  ("abandon",   keepGoing abandonCmd,           noCompletion),
  ("break",     keepGoing breakCmd,             completeIdentifier),
  ("back",      keepGoing backCmd,              noCompletion),
  ("browse",    keepGoing' (browseCmd False),   completeModule),
  ("browse!",   keepGoing' (browseCmd True),    completeModule),
  ("cd",        keepGoing' changeDirectory,     completeFilename),
  ("check",     keepGoing' checkModule,         completeHomeModule),
  ("continue",  keepGoing continueCmd,          noCompletion),
  ("cmd",       keepGoing cmdCmd,               completeExpression),
  ("ctags",     keepGoing createCTagsWithLineNumbersCmd, completeFilename),
  ("ctags!",    keepGoing createCTagsWithRegExesCmd, completeFilename),
  ("def",       keepGoing (defineMacro False),  completeExpression),
  ("def!",      keepGoing (defineMacro True),   completeExpression),
  ("delete",    keepGoing deleteCmd,            noCompletion),
  ("edit",      keepGoing editFile,             completeFilename),
  ("etags",     keepGoing createETagsFileCmd,   completeFilename),
  ("force",     keepGoing forceCmd,             completeExpression),
  ("forward",   keepGoing forwardCmd,           noCompletion),
  ("help",      keepGoing help,                 noCompletion),
  ("history",   keepGoing historyCmd,           noCompletion),
  ("info",      keepGoing' info,                completeIdentifier),
  ("kind",      keepGoing' kindOfType,          completeIdentifier),
  ("load",      keepGoingPaths loadModule_,     completeHomeModuleOrFile),
  ("list",      keepGoing' listCmd,             noCompletion),
  ("module",    keepGoing setContext,           completeSetModule),
  ("main",      keepGoing runMain,              completeFilename),
  ("print",     keepGoing printCmd,             completeExpression),
  ("quit",      quit,                           noCompletion),
  ("reload",    keepGoing' reloadModule,        noCompletion),
  ("run",       keepGoing runRun,               completeFilename),
  ("set",       keepGoing setCmd,               completeSetOptions),
  ("show",      keepGoing showCmd,              completeShowOptions),
  ("sprint",    keepGoing sprintCmd,            completeExpression),
  ("step",      keepGoing stepCmd,              completeIdentifier),
  ("steplocal", keepGoing stepLocalCmd,         completeIdentifier),
  ("stepmodule",keepGoing stepModuleCmd,        completeIdentifier),
  ("type",      keepGoing' typeOfExpr,          completeExpression),
  ("trace",     keepGoing traceCmd,             completeExpression),
  ("undef",     keepGoing undefineMacro,        completeMacro),
  ("unset",     keepGoing unsetOptions,         completeSetOptions)
  ]

word_break_chars :: String
word_break_chars = let symbols = "!#$%&*+/<=>?@\\^|-~"
                       specials = "(),;[]`{}"
                       spaces = " \t\n"
                   in spaces ++ specials ++ symbols

flagWordBreakChars :: String
flagWordBreakChars = " \t\n"

keepGoing :: (String -> GHCi ()) -> (String -> InputT GHCi Bool)
keepGoing a str = keepGoing' (lift . a) str

keepGoing' :: Monad m => (String -> m ()) -> String -> m Bool
keepGoing' a str = a str >> return False

keepGoingPaths :: ([FilePath] -> InputT GHCi ()) -> (String -> InputT GHCi Bool)
keepGoingPaths a str
 = do case toArgs str of
          Left err -> Encoding.encode err >>= liftIO . BS.hPutStrLn stderr
          Right args -> a args
      return False

shortHelpText :: String
shortHelpText = "use :? for help.\n"

helpText :: String
helpText =
 " Commands available from the prompt:\n" ++
 "\n" ++
 "   <statement>                 evaluate/run <statement>\n" ++
 "   :                           repeat last command\n" ++
 "   :{\\n ..lines.. \\n:}\\n       multiline command\n" ++
 "   :add [*]<module> ...        add module(s) to the current target set\n" ++
 "   :browse[!] [[*]<mod>]       display the names defined by module <mod>\n" ++
 "                               (!: more details; *: all top-level names)\n" ++
 "   :cd <dir>                   change directory to <dir>\n" ++
 "   :cmd <expr>                 run the commands returned by <expr>::IO String\n" ++
 "   :ctags[!] [<file>]          create tags file for Vi (default: \"tags\")\n" ++
 "                               (!: use regex instead of line number)\n" ++
 "   :def <cmd> <expr>           define a command :<cmd>\n" ++
 "   :edit <file>                edit file\n" ++
 "   :edit                       edit last module\n" ++
 "   :etags [<file>]             create tags file for Emacs (default: \"TAGS\")\n" ++
 "   :help, :?                   display this list of commands\n" ++
 "   :info [<name> ...]          display information about the given names\n" ++
 "   :kind <type>                show the kind of <type>\n" ++
 "   :load [*]<module> ...       load module(s) and their dependents\n" ++
 "   :main [<arguments> ...]     run the main function with the given arguments\n" ++
 "   :module [+/-] [*]<mod> ...  set the context for expression evaluation\n" ++
 "   :quit                       exit GHCi\n" ++
 "   :reload                     reload the current module set\n" ++
 "   :run function [<arguments> ...] run the function with the given arguments\n" ++
 "   :type <expr>                show the type of <expr>\n" ++
 "   :undef <cmd>                undefine user-defined command :<cmd>\n" ++
 "   :!<command>                 run the shell command <command>\n" ++
 "\n" ++
 " -- Commands for debugging:\n" ++
 "\n" ++
 "   :abandon                    at a breakpoint, abandon current computation\n" ++
 "   :back                       go back in the history (after :trace)\n" ++
 "   :break [<mod>] <l> [<col>]  set a breakpoint at the specified location\n" ++
 "   :break <name>               set a breakpoint on the specified function\n" ++
 "   :continue                   resume after a breakpoint\n" ++
 "   :delete <number>            delete the specified breakpoint\n" ++
 "   :delete *                   delete all breakpoints\n" ++
 "   :force <expr>               print <expr>, forcing unevaluated parts\n" ++
 "   :forward                    go forward in the history (after :back)\n" ++
 "   :history [<n>]              after :trace, show the execution history\n" ++
 "   :list                       show the source code around current breakpoint\n" ++
 "   :list identifier            show the source code for <identifier>\n" ++
 "   :list [<module>] <line>     show the source code around line number <line>\n" ++
 "   :print [<name> ...]         prints a value without forcing its computation\n" ++
 "   :sprint [<name> ...]        simplifed version of :print\n" ++
 "   :step                       single-step after stopping at a breakpoint\n"++
 "   :step <expr>                single-step into <expr>\n"++
 "   :steplocal                  single-step within the current top-level binding\n"++
 "   :stepmodule                 single-step restricted to the current module\n"++
 "   :trace                      trace after stopping at a breakpoint\n"++
 "   :trace <expr>               evaluate <expr> with tracing on (see :history)\n"++

 "\n" ++
 " -- Commands for changing settings:\n" ++
 "\n" ++
 "   :set <option> ...           set options\n" ++
 "   :set args <arg> ...         set the arguments returned by System.getArgs\n" ++
 "   :set prog <progname>        set the value returned by System.getProgName\n" ++
 "   :set prompt <prompt>        set the prompt used in GHCi\n" ++
 "   :set editor <cmd>           set the command used for :edit\n" ++
 "   :set stop [<n>] <cmd>       set the command to run when a breakpoint is hit\n" ++
 "   :unset <option> ...         unset options\n" ++
 "\n" ++
 "  Options for ':set' and ':unset':\n" ++
 "\n" ++
 "    +m            allow multiline commands\n" ++             
 "    +r            revert top-level expressions after each evaluation\n" ++
 "    +s            print timing/memory stats after each evaluation\n" ++
 "    +t            print type after evaluation\n" ++
 "    -<flags>      most GHC command line flags can also be set here\n" ++
 "                         (eg. -v2, -fglasgow-exts, etc.)\n" ++
 "                    for GHCi-specific flags, see User's Guide,\n"++
 "                    Flag reference, Interactive-mode options\n" ++
 "\n" ++
 " -- Commands for displaying information:\n" ++
 "\n" ++
 "   :show bindings              show the current bindings made at the prompt\n" ++
 "   :show breaks                show the active breakpoints\n" ++
 "   :show context               show the breakpoint context\n" ++
 "   :show modules               show the currently loaded modules\n" ++
 "   :show packages              show the currently active package flags\n" ++
 "   :show languages             show the currently active language flags\n" ++
 "   :show <setting>             show value of <setting>, which is one of\n" ++
 "                                  [args, prog, prompt, editor, stop]\n" ++
 "\n" 

findEditor :: IO String
findEditor = do
  getEnv "EDITOR" 
    `catchIO` \_ -> do
#if mingw32_HOST_OS
        win <- System.Win32.getWindowsDirectory
        return (win </> "notepad.exe")
#else
        return ""
#endif

foreign import ccall unsafe "rts_isProfiled" isProfiled :: IO CInt

default_progname, default_prompt, default_stop :: String
default_progname = "<interactive>"
default_prompt = "%s> "
default_stop = ""

default_args :: [String]
default_args = []

interactiveUI :: [(FilePath, Maybe Phase)] -> Maybe [String]
              -> Ghc ()
interactiveUI srcs maybe_exprs = do
   i <- liftIO $ isProfiled
   when (i /= 0) $ 
     ghcError (InstallationError "GHCi cannot be used when compiled with -prof")

   _ <- liftIO $ newStablePtr stdin
   _ <- liftIO $ newStablePtr stdout
   _ <- liftIO $ newStablePtr stderr

   initInterpBuffering

   liftIO $ when (isNothing maybe_exprs) $ do

        turnOffBuffering
        hFlush stdout
        hSetBuffering stdout NoBuffering
        hSetBuffering stdin NoBuffering
#if defined(mingw32_HOST_OS)
        hSetEncoding stdin utf8
#endif

   prel_mod <- GHC.lookupModule (GHC.mkModuleName "Prelude") Nothing
   GHC.setContext [] [(prel_mod, Nothing)]

   default_editor <- liftIO $ findEditor

   startGHCi (runGHCi srcs maybe_exprs)
        GHCiState{ progname = default_progname,
                   args = default_args,
                   prompt = default_prompt,
                   stop = default_stop,
                   editor = default_editor,
                   options = [],
                   prelude = prel_mod,
                   break_ctr = 0,
                   breaks = [],
                   tickarrays = emptyModuleEnv,
                   last_command = Nothing,
                   cmdqueue = [],
                   remembered_ctx = [],
                   ghc_e = isJust maybe_exprs
                 }

   return ()

withGhcAppData :: (FilePath -> IO a) -> IO a -> IO a
withGhcAppData right left = do
    either_dir <- tryIO (getAppUserDataDirectory "ghc")
    case either_dir of
        Right dir ->
            do createDirectoryIfMissing False dir `catchIO` \_ -> return ()
               right dir
        _ -> left

runGHCi :: [(FilePath, Maybe Phase)] -> Maybe [String] -> GHCi ()
runGHCi paths maybe_exprs = do
  let
   read_dot_files = not opt_IgnoreDotGhci

   current_dir = return (Just ".ghci")

   app_user_dir = liftIO $ withGhcAppData
                    (\dir -> return (Just (dir </> "ghci.conf")))
                    (return Nothing)

   home_dir = do
    either_dir <- liftIO $ tryIO (getEnv "HOME")
    case either_dir of
      Right home -> return (Just (home </> ".ghci"))
      _ -> return Nothing

   canonicalizePath' :: FilePath -> IO (Maybe FilePath)
   canonicalizePath' fp = liftM Just (canonicalizePath fp)
                `catchIO` \_ -> return Nothing

   sourceConfigFile :: FilePath -> GHCi ()
   sourceConfigFile file = do
     exists <- liftIO $ doesFileExist file
     when exists $ do
       dir_ok  <- liftIO $ checkPerms (getDirectory file)
       file_ok <- liftIO $ checkPerms file
       when (dir_ok && file_ok) $ do
         either_hdl <- liftIO $ tryIO (openFile file ReadMode)
         case either_hdl of
           Left _e   -> return ()
           Right hdl ->
               do runInputTWithPrefs defaultPrefs defaultSettings $
                            runCommands $ fileLoop hdl
                  liftIO (hClose hdl `catchIO` \_ -> return ())
     where
      getDirectory f = case takeDirectory f of "" -> "."; d -> d

  when (read_dot_files) $ do
    mcfgs0 <- sequence [ current_dir, app_user_dir, home_dir ]
    mcfgs <- liftIO $ mapM canonicalizePath' (catMaybes mcfgs0)
    mapM_ sourceConfigFile $ nub $ catMaybes mcfgs

  when (not (null paths)) $ do
     ok <- ghciHandle (\e -> do showException e; return Failed) $
                runInputTWithPrefs defaultPrefs defaultSettings $ do
                    let (filePaths, phases) = unzip paths
                    filePaths' <- mapM (Encoding.decode . BS.pack) filePaths
                    loadModule (zip filePaths' phases)
     when (isJust maybe_exprs && failed ok) $
        liftIO (exitWith (ExitFailure 1))

  is_tty <- liftIO (hIsTerminalDevice stdin)
  dflags <- getDynFlags
  let show_prompt = verbosity dflags > 0 || is_tty

  case maybe_exprs of
        Nothing ->
          do
            runGHCiInput $ runCommands $ nextInputLine show_prompt is_tty
        Just exprs -> do
            enqueueCommands exprs
            let handle e = do st <- getGHCiState
                              flushInterpBuffers
                              liftIO $ withProgName (progname st)
                                     $ topHandler e
            runInputTWithPrefs defaultPrefs defaultSettings $ do
                runCommands' handle (return Nothing)

  liftIO $ when (verbosity dflags > 0) $ putStrLn "Leaving GHCi."

runGHCiInput :: InputT GHCi a -> GHCi a
runGHCiInput f = do
    histFile <- liftIO $ withGhcAppData (\dir -> return (Just (dir </> "ghci_history")))
                                        (return Nothing)
    let settings = setComplete ghciCompleteWord
                    $ defaultSettings {historyFile = histFile}
    runInputT settings f

nextInputLine :: Bool -> Bool -> InputT GHCi (Maybe String)
nextInputLine show_prompt is_tty
  | is_tty = do
    prompt <- if show_prompt then lift mkPrompt else return ""
    getInputLine prompt
  | otherwise = do
    when show_prompt $ lift mkPrompt >>= liftIO . putStr
    fileLoop stdin

checkPerms :: String -> IO Bool
#ifdef mingw32_HOST_OS
checkPerms _ =
  return True
#else
checkPerms name =
  handleIO (\_ -> return False) $ do
     st <- getFileStatus name
     me <- getRealUserID
     if fileOwner st /= me then do
   	putStrLn $ "WARNING: " ++ name ++ " is owned by someone else, IGNORING!"
   	return False
      else do
   	let mode =  System.Posix.fileMode st
   	if (groupWriteMode == (mode `intersectFileModes` groupWriteMode))
   	   || (otherWriteMode == (mode `intersectFileModes` otherWriteMode)) 
   	   then do
   	       putStrLn $ "*** WARNING: " ++ name ++ 
   			  " is writable by someone else, IGNORING!"
   	       return False
   	  else return True
#endif

fileLoop :: MonadIO m => Handle -> InputT m (Maybe String)
fileLoop hdl = do
   l <- liftIO $ tryIO $ hGetLine hdl
   case l of
        Left e | isEOFError e              -> return Nothing
               | InvalidArgument <- etype  -> return Nothing
               | otherwise                 -> liftIO $ ioError e
                where etype = ioeGetErrorType e
        Right l -> return (Just l)

mkPrompt :: GHCi String
mkPrompt = do
  (toplevs,exports) <- GHC.getContext
  resumes <- GHC.getResumeContext

  context_bit <-
        case resumes of
            [] -> return empty
            r:_ -> do
                let ix = GHC.resumeHistoryIx r
                if ix == 0
                   then return (brackets (ppr (GHC.resumeSpan r)) <> space)
                   else do
                        let hist = GHC.resumeHistory r !! (ix-1)
                        span <- GHC.getHistorySpan hist
                        return (brackets (ppr (negate ix) <> char ':' 
                                          <+> ppr span) <> space)
  let
        dots | _:rs <- resumes, not (null rs) = text "... "
             | otherwise = empty

        modules_bit = 
             hsep (map (\m -> char '*'  <> ppr (GHC.moduleName m)) toplevs) <+>
             hsep (map (ppr . GHC.moduleName) (nub (map fst exports)))

        deflt_prompt = dots <> context_bit <> modules_bit

        f ('%':'s':xs) = deflt_prompt <> f xs
        f ('%':'%':xs) = char '%' <> f xs
        f (x:xs) = char x <> f xs
        f [] = empty
  st <- getGHCiState
  return (showSDoc (f (prompt st)))

queryQueue :: GHCi (Maybe String)
queryQueue = do
  st <- getGHCiState
  case cmdqueue st of
    []   -> return Nothing
    c:cs -> do setGHCiState st{ cmdqueue = cs }
               return (Just c)

runCommands :: InputT GHCi (Maybe String) -> InputT GHCi ()
runCommands = runCommands' handler

runCommands' :: (SomeException -> GHCi Bool)
             -> InputT GHCi (Maybe String) -> InputT GHCi ()
runCommands' eh getCmd = do
    b <- ghandle (\e -> case fromException e of
                          Just UserInterrupt -> return $ Just False
                          _ -> case fromException e of
                                 Just ghc_e ->
                                   do liftIO (print (ghc_e :: GhcException))
                                      return Nothing
                                 _other ->
                                   liftIO (Exception.throwIO e))
            (runOneCommand eh getCmd)
    case b of
      Nothing -> return ()
      Just _  -> runCommands' eh getCmd

runOneCommand :: (SomeException -> GHCi Bool) -> InputT GHCi (Maybe String)
            -> InputT GHCi (Maybe Bool)
runOneCommand eh getCmd = do
  mb_cmd <- noSpace (lift queryQueue)
  mb_cmd <- maybe (noSpace getCmd) (return . Just) mb_cmd
  case mb_cmd of
    Nothing -> return Nothing
    Just c  -> ghciHandle (\e -> lift $ eh e >>= return . Just) $
             handleSourceError printErrorAndKeepGoing
               (doCommand c)
  where
    printErrorAndKeepGoing err = do
        GHC.printException err
        return $ Just True

    noSpace q = q >>= maybe (return Nothing)
                            (\c->case removeSpaces c of 
                                   ""   -> noSpace q
                                   ":{" -> multiLineCmd q
                                   c    -> return (Just c) )
    multiLineCmd q = do
      st <- lift getGHCiState
      let p = prompt st
      lift $ setGHCiState st{ prompt = "%s| " }
      mb_cmd <- collectCommand q ""
      lift $ getGHCiState >>= \st->setGHCiState st{ prompt = p }
      return mb_cmd
    collectCommand q c = q >>= 
      maybe (liftIO (ioError collectError))
            (\l->if removeSpaces l == ":}" 
                 then return (Just $ removeSpaces c) 
                 else collectCommand q (c ++ "\n" ++ map normSpace l))
      where normSpace '\r' = ' '
            normSpace   c  = c
    collectError = userError "unterminated multiline command :{ .. :}"
    doCommand (':' : cmd) = do
      result <- specialCommand cmd
      case result of
        True -> return Nothing
        _    -> return $ Just True
    doCommand stmt        = do 
      ml <- lift $ isOptionSet Multiline
      if ml
        then do 
          mb_stmt <- checkInputForLayout stmt 1 getCmd
          case mb_stmt of
            Nothing      -> return $ Just True
            Just ml_stmt -> do
              result <- timeIt $ lift $ runStmt ml_stmt GHC.RunToCompletion
              return $ Just result
        else do
          result <- timeIt $ lift $ runStmt stmt GHC.RunToCompletion
          return $ Just result

checkInputForLayout :: String -> Int -> InputT GHCi (Maybe String)
                    -> InputT GHCi (Maybe String)
checkInputForLayout stmt line_number getStmt = do
   dflags' <- lift $ getDynFlags
   let dflags = xopt_set dflags' Opt_AlternativeLayoutRule
   st <- lift $ getGHCiState
   let buf =  stringToStringBuffer stmt
       loc  = mkSrcLoc (fsLit (progname st)) line_number 1
       pstate = Lexer.mkPState dflags buf loc
   case Lexer.unP goToEnd pstate of
     (Lexer.POk _ False) -> return $ Just stmt
     _other              -> do
       st <- lift getGHCiState
       let p = prompt st
       lift $ setGHCiState st{ prompt = "%s| " }
       mb_stmt <- ghciHandle (\ex -> case fromException ex of
                            Just UserInterrupt -> return Nothing
                            _ -> case fromException ex of
                                 Just ghc_e ->
                                   do liftIO (print (ghc_e :: GhcException))
                                      return Nothing
                                 _other -> liftIO (Exception.throwIO ex)) 
                     getStmt
       lift $ getGHCiState >>= \st->setGHCiState st{ prompt = p }
       case mb_stmt of
         Nothing  -> return Nothing
         Just str -> if str == ""
           then return $ Just stmt
           else checkInputForLayout (stmt++"\n"++str) (line_number+1) getStmt
     where goToEnd = do
             eof <- Lexer.nextIsEOF
             if eof 
               then Lexer.activeContext
               else Lexer.lexer return >> goToEnd

enqueueCommands :: [String] -> GHCi ()
enqueueCommands cmds = do
  st <- getGHCiState
  setGHCiState st{ cmdqueue = cmds ++ cmdqueue st }

runStmt :: String -> SingleStep -> GHCi Bool
runStmt stmt step
 | null (filter (not.isSpace) stmt)
 = return False
 | "import " `isPrefixOf` stmt
 = do newContextCmd (Import stmt); return False
 | otherwise
 = do
      _ <- liftIO $ tryIO $ hFlushAll stdin
      result <- GhciMonad.runStmt stmt step
      afterRunStmt (const True) result

afterRunStmt :: (SrcSpan -> Bool) -> GHC.RunResult -> GHCi Bool
afterRunStmt _ (GHC.RunException e) = throw e
afterRunStmt step_here run_result = do
  resumes <- GHC.getResumeContext
  case run_result of
     GHC.RunOk names -> do
        show_types <- isOptionSet ShowType
        when show_types $ printTypeOfNames names
     GHC.RunBreak _ names mb_info
         | isNothing  mb_info ||
           step_here (GHC.resumeSpan $ head resumes) -> do
               mb_id_loc <- toBreakIdAndLocation mb_info
               let breakCmd = maybe "" ( \(_,l) -> onBreakCmd l ) mb_id_loc
               if (null breakCmd)
                 then printStoppedAtBreakInfo (head resumes) names
                 else enqueueCommands [breakCmd]
               st <- getGHCiState
               enqueueCommands [stop st]
               return ()
         | otherwise -> resume step_here GHC.SingleStep >>=
                        afterRunStmt step_here >> return ()
     _ -> return ()

  flushInterpBuffers
  liftIO installSignalHandlers
  b <- isOptionSet RevertCAFs
  when b revertCAFs

  return (case run_result of GHC.RunOk _ -> True; _ -> False)

toBreakIdAndLocation ::
  Maybe GHC.BreakInfo -> GHCi (Maybe (Int, BreakLocation))
toBreakIdAndLocation Nothing = return Nothing
toBreakIdAndLocation (Just info) = do
  let mod = GHC.breakInfo_module info
      nm  = GHC.breakInfo_number info
  st <- getGHCiState
  return $ listToMaybe [ id_loc | id_loc@(_,loc) <- breaks st,
                                  breakModule loc == mod,
                                  breakTick loc == nm ]

printStoppedAtBreakInfo :: Resume -> [Name] -> GHCi ()
printStoppedAtBreakInfo resume names = do
  printForUser $ ptext (sLit "Stopped at") <+>
    ppr (GHC.resumeSpan resume)
  let namesSorted = sortBy compareNames names
  tythings <- catMaybes `liftM` mapM GHC.lookupName namesSorted
  docs <- pprTypeAndContents [id | AnId id <- tythings]
  printForUserPartWay docs

printTypeOfNames :: [Name] -> GHCi ()
printTypeOfNames names
 = mapM_ (printTypeOfName ) $ sortBy compareNames names

compareNames :: Name -> Name -> Ordering
n1 `compareNames` n2 = compareWith n1 `compare` compareWith n2
    where compareWith n = (getOccString n, getSrcSpan n)

printTypeOfName :: Name -> GHCi ()
printTypeOfName n
   = do maybe_tything <- GHC.lookupName n
        case maybe_tything of
            Nothing    -> return ()
            Just thing -> printTyThing thing

data MaybeCommand = GotCommand Command | BadCommand | NoLastCommand

specialCommand :: String -> InputT GHCi Bool
specialCommand ('!':str) = lift $ shellEscape (dropWhile isSpace str)
specialCommand str = do
  let (cmd,rest) = break isSpace str
  maybe_cmd <- lift $ lookupCommand cmd
  case maybe_cmd of
    GotCommand (_,f,_) -> f (dropWhile isSpace rest)
    BadCommand ->
      do liftIO $ hPutStr stdout ("unknown command ':" ++ cmd ++ "'\n"
                           ++ shortHelpText)
         return False
    NoLastCommand ->
      do liftIO $ hPutStr stdout ("there is no last command to perform\n"
                           ++ shortHelpText)
         return False

lookupCommand :: String -> GHCi (MaybeCommand)
lookupCommand "" = do
  st <- getGHCiState
  case last_command st of
      Just c -> return $ GotCommand c
      Nothing -> return NoLastCommand
lookupCommand str = do
  mc <- liftIO $ lookupCommand' str
  st <- getGHCiState
  setGHCiState st{ last_command = mc }
  return $ case mc of
           Just c -> GotCommand c
           Nothing -> BadCommand

lookupCommand' :: String -> IO (Maybe Command)
lookupCommand' ":" = return Nothing
lookupCommand' str' = do
  macros <- readIORef macros_ref
  let{ (str, cmds) = case str' of
      ':' : rest -> (rest, builtin_commands)
      _ -> (str', macros ++ builtin_commands) }
  return $ case [ c | c <- cmds, str == cmdName c ] of
           c:_ -> Just c
           [] -> case [ c | c@(s,_,_) <- cmds, str `isPrefixOf` s ] of
                 [] -> Nothing
                 c:_ -> Just c

getCurrentBreakSpan :: GHCi (Maybe SrcSpan)
getCurrentBreakSpan = do
  resumes <- GHC.getResumeContext
  case resumes of
    [] -> return Nothing
    (r:_) -> do
        let ix = GHC.resumeHistoryIx r
        if ix == 0
           then return (Just (GHC.resumeSpan r))
           else do
                let hist = GHC.resumeHistory r !! (ix-1)
                span <- GHC.getHistorySpan hist
                return (Just span)

getCurrentBreakModule :: GHCi (Maybe Module)
getCurrentBreakModule = do
  resumes <- GHC.getResumeContext
  case resumes of
    [] -> return Nothing
    (r:_) -> do
        let ix = GHC.resumeHistoryIx r
        if ix == 0
           then return (GHC.breakInfo_module `liftM` GHC.resumeBreakInfo r)
           else do
                let hist = GHC.resumeHistory r !! (ix-1)
                return $ Just $ GHC.getHistoryModule  hist

noArgs :: GHCi () -> String -> GHCi ()
noArgs m "" = m
noArgs _ _  = liftIO $ putStrLn "This command takes no arguments"

help :: String -> GHCi ()
help _ = liftIO (putStr helpText)

info :: String -> InputT GHCi ()
info "" = ghcError (CmdLineError "syntax: ':i <thing-you-want-info-about>'")
info s  = handleSourceError GHC.printException $
          do { let names = words s
	     ; dflags <- getDynFlags
	     ; let pefas = dopt Opt_PrintExplicitForalls dflags
	     ; mapM_ (infoThing pefas) names }
  where
    infoThing pefas str = do
	names     <- GHC.parseName str
	mb_stuffs <- mapM GHC.getInfo names
	let filtered = filterOutChildren (\(t,_f,_i) -> t) (catMaybes mb_stuffs)
	unqual <- GHC.getPrintUnqual
	liftIO $ putStrLn $ showSDocForUser unqual $
     		     vcat (intersperse (text "") $
		           map (pprInfo pefas) filtered)

filterOutChildren :: (a -> TyThing) -> [a] -> [a]
filterOutChildren get_thing xs 
  = filterOut has_parent xs
  where
    all_names = mkNameSet (map (getName . get_thing) xs)
    has_parent x = case pprTyThingParent_maybe (get_thing x) of
                     Just p  -> getName p `elemNameSet` all_names
                     Nothing -> False

pprInfo :: PrintExplicitForalls -> (TyThing, Fixity, [GHC.Instance]) -> SDoc
pprInfo pefas (thing, fixity, insts)
  =  pprTyThingInContextLoc pefas thing
  $$ show_fixity fixity
  $$ vcat (map GHC.pprInstance insts)
  where
    show_fixity fix 
	| fix == GHC.defaultFixity = empty
	| otherwise		   = ppr fix <+> ppr (GHC.getName thing)

runMain :: String -> GHCi ()
runMain s = case toArgs s of
            Left err   -> liftIO (hPutStrLn stderr err)
            Right args ->
                do dflags <- getDynFlags
                   case mainFunIs dflags of
                       Nothing -> doWithArgs args "main"
                       Just f  -> doWithArgs args f

runRun :: String -> GHCi ()
runRun s = case toCmdArgs s of
           Left err          -> liftIO (hPutStrLn stderr err)
           Right (cmd, args) -> doWithArgs args cmd

doWithArgs :: [String] -> String -> GHCi ()
doWithArgs args cmd = enqueueCommands ["System.Environment.withArgs " ++
                                       show args ++ " (" ++ cmd ++ ")"]

addModule :: [FilePath] -> InputT GHCi ()
addModule files = do
  lift revertCAFs
  files <- mapM expandPath files
  targets <- mapM (\m -> GHC.guessTarget m Nothing) files
  mapM_ GHC.removeTarget [ tid | Target tid _ _ <- targets ]
  mapM_ GHC.addTarget targets
  prev_context <- GHC.getContext
  ok <- trySuccess $ GHC.load LoadAllTargets
  afterLoad ok False prev_context

changeDirectory :: String -> InputT GHCi ()
changeDirectory "" = do
  either_dir <- liftIO $ tryIO getHomeDirectory
  case either_dir of
     Left _e -> return ()
     Right dir -> changeDirectory dir
changeDirectory dir = do
  graph <- GHC.getModuleGraph
  when (not (null graph)) $
        liftIO $ putStrLn "Warning: changing directory causes all loaded modules to be unloaded,\nbecause the search path has changed."
  prev_context <- GHC.getContext
  GHC.setTargets []
  _ <- GHC.load LoadAllTargets
  lift $ setContextAfterLoad prev_context False []
  GHC.workingDirectoryChanged
  dir <- expandPath dir
  liftIO $ setCurrentDirectory dir

trySuccess :: GHC.GhcMonad m => m SuccessFlag -> m SuccessFlag
trySuccess act =
    handleSourceError (\e -> do GHC.printException e
                                return Failed) $ do
      act

editFile :: String -> GHCi ()
editFile str =
  do file <- if null str then chooseEditFile else return str
     st <- getGHCiState
     let cmd = editor st
     when (null cmd) 
       $ ghcError (CmdLineError "editor not set, use :set editor")
     _ <- liftIO $ system (cmd ++ ' ':file)
     return ()

chooseEditFile :: GHCi String
chooseEditFile =
  do let hasFailed x = fmap not $ GHC.isLoaded $ GHC.ms_mod_name x

     graph <- GHC.getModuleGraph
     failed_graph <- filterM hasFailed graph
     let order g  = flattenSCCs $ GHC.topSortModuleGraph True g Nothing
         pick xs  = case xs of
                      x : _ -> GHC.ml_hs_file (GHC.ms_location x)
                      _     -> Nothing

     case pick (order failed_graph) of
       Just file -> return file
       Nothing   -> 
         do targets <- GHC.getTargets
            case msum (map fromTarget targets) of
              Just file -> return file
              Nothing   -> ghcError (CmdLineError "No files to edit.")
          
  where fromTarget (GHC.Target (GHC.TargetFile f _) _ _) = Just f
        fromTarget _ = Nothing

defineMacro :: Bool -> String -> GHCi ()
defineMacro _ (':':_) =
  liftIO $ putStrLn "macro name cannot start with a colon"
defineMacro overwrite s = do
  let (macro_name, definition) = break isSpace s
  macros <- liftIO (readIORef macros_ref)
  let defined = map cmdName macros
  if (null macro_name) 
	then if null defined
                then liftIO $ putStrLn "no macros defined"
                else liftIO $ putStr ("the following macros are defined:\n" ++
                                      unlines defined)
	else do
  if (not overwrite && macro_name `elem` defined)
	then ghcError (CmdLineError 
		("macro '" ++ macro_name ++ "' is already defined"))
	else do

  let filtered = [ cmd | cmd <- macros, cmdName cmd /= macro_name ]

  let new_expr = '(' : definition ++ ") :: String -> IO String"

  handleSourceError (\e -> GHC.printException e) $
   do
    hv <- GHC.compileExpr new_expr
    liftIO (writeIORef macros_ref
            (filtered ++ [(macro_name, lift . runMacro hv, noCompletion)]))

runMacro :: GHC.HValue -> String -> GHCi Bool
runMacro fun s = do
  str <- liftIO ((unsafeCoerce# fun :: String -> IO String) s)
  seqList str (return ())
  enqueueCommands (lines str)
  return False

undefineMacro :: String -> GHCi ()
undefineMacro str = mapM_ undef (words str) 
 where undef macro_name = do
        cmds <- liftIO (readIORef macros_ref)
        if (macro_name `notElem` map cmdName cmds) 
      	   then ghcError (CmdLineError 
      		("macro '" ++ macro_name ++ "' is not defined"))
      	   else do
            liftIO (writeIORef macros_ref (filter ((/= macro_name) . cmdName) cmds))

cmdCmd :: String -> GHCi ()
cmdCmd str = do
  let expr = '(' : str ++ ") :: IO String"
  handleSourceError (\e -> GHC.printException e) $
   do
    hv <- GHC.compileExpr expr
    cmds <- liftIO $ (unsafeCoerce# hv :: IO String)
    enqueueCommands (lines cmds)
    return ()

loadModuleName :: GHC.GhcMonad m => ImportDecl RdrName -> m Module
loadModuleName = flip GHC.findModule Nothing . unLoc . ideclName

loadModule :: [(FilePath, Maybe Phase)] -> InputT GHCi SuccessFlag
loadModule fs = timeIt (loadModule' fs)

loadModule_ :: [FilePath] -> InputT GHCi ()
loadModule_ fs = loadModule (zip fs (repeat Nothing)) >> return ()

loadModule' :: [(FilePath, Maybe Phase)] -> InputT GHCi SuccessFlag
loadModule' files = do
  prev_context <- GHC.getContext

  _ <- GHC.abandonAll
  lift discardActiveBreakPoints
  GHC.setTargets []
  _ <- GHC.load LoadAllTargets

  let (filenames, phases) = unzip files
  exp_filenames <- mapM expandPath filenames
  let files' = zip exp_filenames phases
  targets <- mapM (uncurry GHC.guessTarget) files'

  GHC.setTargets targets
  doLoad False prev_context LoadAllTargets

checkModule :: String -> InputT GHCi ()
checkModule m = do
  let modl = GHC.mkModuleName m
  prev_context <- GHC.getContext
  ok <- handleSourceError (\e -> GHC.printException e >> return False) $ do
          r <- GHC.typecheckModule =<< GHC.parseModule =<< GHC.getModSummary modl
          liftIO $ putStrLn $ showSDoc $
	   case GHC.moduleInfo r of
	     cm | Just scope <- GHC.modInfoTopLevelScope cm ->
		let
		    (local,global) = ASSERT( all isExternalName scope )
		    		     partition ((== modl) . GHC.moduleName . GHC.nameModule) scope
		in
			(text "global names: " <+> ppr global) $$
		        (text "local  names: " <+> ppr local)
	     _ -> empty
          return True
  afterLoad (successIf ok) False prev_context

reloadModule :: String -> InputT GHCi ()
reloadModule m = do
  prev_context <- GHC.getContext
  _ <- doLoad True prev_context $
        if null m then LoadAllTargets 
                  else LoadUpTo (GHC.mkModuleName m)
  return ()

doLoad :: Bool -> ([Module],[(Module, Maybe (ImportDecl RdrName))]) -> LoadHowMuch -> InputT GHCi SuccessFlag
doLoad retain_context prev_context howmuch = do
  lift discardActiveBreakPoints
  ok <- trySuccess $ GHC.load howmuch
  afterLoad ok retain_context prev_context
  return ok

afterLoad :: SuccessFlag -> Bool -> ([Module],[(Module, Maybe (ImportDecl RdrName))]) -> InputT GHCi ()
afterLoad ok retain_context prev_context = do
  lift revertCAFs
  lift discardTickArrays
  loaded_mod_summaries <- getLoadedModules
  let loaded_mods = map GHC.ms_mod loaded_mod_summaries
      loaded_mod_names = map GHC.moduleName loaded_mods
  modulesLoadedMsg ok loaded_mod_names

  lift $ setContextAfterLoad prev_context retain_context loaded_mod_summaries

setContextAfterLoad :: ([Module],[(Module, Maybe (ImportDecl RdrName))]) -> Bool -> [GHC.ModSummary] -> GHCi ()
setContextAfterLoad prev keep_ctxt [] = do
  prel_mod <- getPrelude
  setContextKeepingPackageModules prev keep_ctxt ([], [(prel_mod, Nothing)])
setContextAfterLoad prev keep_ctxt ms = do
  targets <- GHC.getTargets
  case [ m | Just m <- map (findTarget ms) targets ] of
	[]    -> 
	  let graph' = flattenSCCs (GHC.topSortModuleGraph True ms Nothing) in
	  load_this (last graph')	  
	(m:_) -> 
	  load_this m
 where
   findTarget ms t
    = case filter (`matches` t) ms of
	[]    -> Nothing
	(m:_) -> Just m

   summary `matches` Target (TargetModule m) _ _
	= GHC.ms_mod_name summary == m
   summary `matches` Target (TargetFile f _) _ _ 
	| Just f' <- GHC.ml_hs_file (GHC.ms_location summary)	= f == f'
   _ `matches` _
	= False

   load_this summary | m <- GHC.ms_mod summary = do
	b <- GHC.moduleIsInterpreted m
	if b then setContextKeepingPackageModules prev keep_ctxt ([m], [])
       	     else do
                prel_mod <- getPrelude
                setContextKeepingPackageModules prev keep_ctxt ([],[(prel_mod,Nothing),(m,Nothing)])

setContextKeepingPackageModules
        :: ([Module],[(Module, Maybe (ImportDecl RdrName))])
        -> Bool
        -> ([Module],[(Module, Maybe (ImportDecl RdrName))])
        -> GHCi ()
setContextKeepingPackageModules prev_context keep_ctxt (as,bs) = do
  let (_,bs0) = prev_context
  prel_mod <- getPrelude
  let pkg_modules = filter ((\p -> not (isHomeModule p) && p /= prel_mod) . fst) bs0
  let bs1 = if null as then nubBy sameFst ((prel_mod,Nothing) : bs) else bs
  GHC.setContext as (nubBy sameFst (bs1 ++ pkg_modules))
  if keep_ctxt
     then do
          st <- getGHCiState
          mapM_ (playCtxtCmd False) (remembered_ctx st)
     else do
          st <- getGHCiState
          setGHCiState st{ remembered_ctx = [] }

isHomeModule :: Module -> Bool
isHomeModule mod = GHC.modulePackageId mod == mainPackageId

sameFst :: (Module, Maybe (ImportDecl RdrName)) -> (Module, Maybe (ImportDecl RdrName)) -> Bool
sameFst x y = fst x == fst y

modulesLoadedMsg :: SuccessFlag -> [ModuleName] -> InputT GHCi ()
modulesLoadedMsg ok mods = do
  dflags <- getDynFlags
  when (verbosity dflags > 0) $ do
   let mod_commas 
	| null mods = text "none."
	| otherwise = hsep (
	    punctuate comma (map ppr mods)) <> text "."
   case ok of
    Failed ->
       liftIO $ putStrLn $ showSDoc (text "Failed, modules loaded: " <> mod_commas)
    Succeeded  ->
       liftIO $ putStrLn $ showSDoc (text "Ok, modules loaded: " <> mod_commas)

typeOfExpr :: String -> InputT GHCi ()
typeOfExpr str 
  = handleSourceError GHC.printException
  $ do
       ty <- GHC.exprType str
       dflags <- getDynFlags
       let pefas = dopt Opt_PrintExplicitForalls dflags
       printForUser $ sep [text str, nest 2 (dcolon <+> pprTypeForUser pefas ty)]

kindOfType :: String -> InputT GHCi ()
kindOfType str 
  = handleSourceError GHC.printException
  $ do
       ty <- GHC.typeKind str
       printForUser $ text str <+> dcolon <+> ppr ty

quit :: String -> InputT GHCi Bool
quit _ = return True

shellEscape :: String -> GHCi Bool
shellEscape str = liftIO (system str >> return False)

browseCmd :: Bool -> String -> InputT GHCi ()
browseCmd bang m = 
  case words m of
    ['*':s] | looksLikeModuleName s -> do 
        m <- lift $ wantInterpretedModule s
        browseModule bang m False
    [s] | looksLikeModuleName s -> do
        m <- lift $ lookupModule s
        browseModule bang m True
    [] -> do
        (as,bs) <- GHC.getContext
        case (as,bs) of
          (as@(_:_), _)   -> browseModule bang (last as) True
          ([],  bs@(_:_)) -> browseModule bang (fst (last bs)) True
          ([], [])  -> ghcError (CmdLineError ":browse: no current module")
    _ -> ghcError (CmdLineError "syntax:  :browse <module>")

browseModule :: Bool -> Module -> Bool -> InputT GHCi ()
browseModule bang modl exports_only = do
  current_unqual <- GHC.getPrintUnqual
  (as,bs) <- GHC.getContext
  prel_mod <- lift getPrelude
  if exports_only then GHC.setContext [] [(prel_mod,Nothing), (modl,Nothing)]
                  else GHC.setContext [modl] []
  target_unqual <- GHC.getPrintUnqual
  GHC.setContext as bs

  let unqual = if bang then current_unqual else target_unqual

  mb_mod_info <- GHC.getModuleInfo modl
  case mb_mod_info of
    Nothing -> ghcError (CmdLineError ("unknown module: " ++
                                GHC.moduleNameString (GHC.moduleName modl)))
    Just mod_info -> do
        dflags <- getDynFlags
        let names
               | exports_only = GHC.modInfoExports mod_info
               | otherwise    = GHC.modInfoTopLevelScope mod_info
                                `orElse` []

            sorted_names = loc_sort local ++ occ_sort external
                where 
                (local,external) = ASSERT( all isExternalName names )
				   partition ((==modl) . nameModule) names
                occ_sort = sortBy (compare `on` nameOccName) 
                loc_sort names
                      | n:_ <- names, isGoodSrcSpan (nameSrcSpan n)
                      = sortBy (compare `on` nameSrcSpan) names
                      | otherwise
                      = occ_sort names

        mb_things <- mapM GHC.lookupName sorted_names
        let filtered_things = filterOutChildren (\t -> t) (catMaybes mb_things)

        rdr_env <- GHC.getGRE

        let pefas              = dopt Opt_PrintExplicitForalls dflags
            things | bang      = catMaybes mb_things
                   | otherwise = filtered_things
            pretty | bang      = pprTyThing
                   | otherwise = pprTyThingInContext

            labels  [] = text "-- not currently imported"
            labels  l  = text $ intercalate "\n" $ map qualifier l
            qualifier  = maybe "-- defined locally" 
                             (("-- imported via "++) . intercalate ", " 
                               . map GHC.moduleNameString)
            importInfo = RdrName.getGRE_NameQualifier_maybes rdr_env
            modNames   = map (importInfo . GHC.getName) things
                                        
            annotate mts = concatMap (\(m,ts)->labels m:ts)
                         $ sortBy cmpQualifiers $ group mts
              where cmpQualifiers = 
                      compare `on` (map (fmap (map moduleNameFS)) . fst)
            group []            = []
            group mts@((m,_):_) = (m,map snd g) : group ng
              where (g,ng) = partition ((==m).fst) mts

        let prettyThings = map (pretty pefas) things
            prettyThings' | bang      = annotate $ zip modNames prettyThings
                          | otherwise = prettyThings
        liftIO $ putStrLn $ showSDocForUser unqual (vcat prettyThings')

newContextCmd :: CtxtCmd -> GHCi ()
newContextCmd cmd = do
  playCtxtCmd True cmd
  st <- getGHCiState
  let cmds = remembered_ctx st
  setGHCiState st{ remembered_ctx = cmds ++ [cmd] }

setContext :: String -> GHCi ()
setContext str
  | all sensible strs = newContextCmd cmd
  | otherwise = ghcError (CmdLineError "syntax:  :module [+/-] [*]M1 ... [*]Mn")
  where
    (cmd, strs) =
        case str of 
                '+':stuff -> rest AddModules stuff
                '-':stuff -> rest RemModules stuff
                stuff     -> rest SetContext stuff

    rest cmd stuff = (cmd as bs, strs)
       where strs = words stuff
             (as,bs) = partitionWith starred strs

    sensible ('*':m) = looksLikeModuleName m
    sensible m       = looksLikeModuleName m

    starred ('*':m) = Left m
    starred m       = Right m

playCtxtCmd:: Bool -> CtxtCmd -> GHCi ()
playCtxtCmd fail cmd = do
    (prev_as,prev_bs) <- GHC.getContext
    case cmd of
        SetContext as bs -> do
          (as',bs') <- do_checks as bs
          prel_mod <- getPrelude
          let bs'' = if null as && prel_mod `notElem` (map fst bs')
                        then (prel_mod,Nothing):bs'
                        else bs'
          GHC.setContext as' bs''

        AddModules as bs -> do
          (as',bs') <- do_checks as bs
          let remaining_as = prev_as \\ (as' ++ map fst bs')
              remaining_bs = deleteAllBy sameFst prev_bs (bs' ++ map contextualize as')
          GHC.setContext (remaining_as ++ as') (remaining_bs ++ bs')

        RemModules as bs -> do
          (as',bs') <- do_checks as bs
          let new_as = prev_as \\ (as' ++ map fst bs')
              new_bs = deleteAllBy sameFst prev_bs (map contextualize as' ++ bs')
          GHC.setContext new_as new_bs

        Import str -> do
          m_idecl <- maybe_fail $ GHC.parseImportDecl str
          case m_idecl of
            Nothing    -> return ()
            Just idecl -> do
              m_mdl <- maybe_fail $ loadModuleName idecl
              case m_mdl of
                Nothing -> return ()
                Just m -> GHC.setContext prev_as (prev_bs ++ [(m, Just idecl)])
    
  where
    maybe_fail | fail      = liftM Just
               | otherwise = trymaybe

    do_checks as bs = do
         as' <- mapM (maybe_fail . wantInterpretedModule) as
         bs' <- mapM (maybe_fail . lookupModule) bs
         return (catMaybes as', map contextualize (catMaybes bs'))

    contextualize x = (x,Nothing)
    deleteAllBy f a b = filter (\x->(not (any (f x) b))) a

trymaybe ::GHCi a -> GHCi (Maybe a)
trymaybe m = do
    r <- ghciTry m
    case r of
      Left _  -> return Nothing
      Right a -> return (Just a)

setCmd :: String -> GHCi ()
setCmd ""
  = do st <- getGHCiState
       let opts = options st
       liftIO $ putStrLn (showSDoc (
   	      text "options currently set: " <> 
   	      if null opts
   		   then text "none."
   		   else hsep (map (\o -> char '+' <> text (optToStr o)) opts)
   	   ))
       dflags <- getDynFlags
       liftIO $ putStrLn (showSDoc (
          vcat (text "GHCi-specific dynamic flag settings:" 
               :map (flagSetting dflags) ghciFlags)
          ))
       liftIO $ putStrLn (showSDoc (
          vcat (text "other dynamic, non-language, flag settings:" 
               :map (flagSetting dflags) others)
          ))
  where flagSetting dflags (str, f, _)
          | dopt f dflags = text "  " <> text "-f"    <> text str
          | otherwise     = text "  " <> text "-fno-" <> text str
        (ghciFlags,others)  = partition (\(_, f, _) -> f `elem` flags)
                                        DynFlags.fFlags
        flags = [Opt_PrintExplicitForalls
                ,Opt_PrintBindResult
                ,Opt_BreakOnException
                ,Opt_BreakOnError
                ,Opt_PrintEvldWithShow
                ] 
setCmd str
  = case getCmd str of
    Right ("args",   rest) ->
        case toArgs rest of
            Left err -> liftIO (hPutStrLn stderr err)
            Right args -> setArgs args
    Right ("prog",   rest) ->
        case toArgs rest of
            Right [prog] -> setProg prog
            _ -> liftIO (hPutStrLn stderr "syntax: :set prog <progname>")
    Right ("prompt", rest) -> setPrompt $ dropWhile isSpace rest
    Right ("editor", rest) -> setEditor $ dropWhile isSpace rest
    Right ("stop",   rest) -> setStop   $ dropWhile isSpace rest
    _ -> case toArgs str of
         Left err -> liftIO (hPutStrLn stderr err)
         Right wds -> setOptions wds

setArgs, setOptions :: [String] -> GHCi ()
setProg, setEditor, setStop, setPrompt :: String -> GHCi ()

setArgs args = do
  st <- getGHCiState
  setGHCiState st{ args = args }

setProg prog = do
  st <- getGHCiState
  setGHCiState st{ progname = prog }

setEditor cmd = do
  st <- getGHCiState
  setGHCiState st{ editor = cmd }

setStop str@(c:_) | isDigit c
  = do let (nm_str,rest) = break (not.isDigit) str
           nm = read nm_str
       st <- getGHCiState
       let old_breaks = breaks st
       if all ((/= nm) . fst) old_breaks
              then printForUser (text "Breakpoint" <+> ppr nm <+>
                                 text "does not exist")
              else do
       let new_breaks = map fn old_breaks
           fn (i,loc) | i == nm   = (i,loc { onBreakCmd = dropWhile isSpace rest })
                      | otherwise = (i,loc)
       setGHCiState st{ breaks = new_breaks }
setStop cmd = do
  st <- getGHCiState
  setGHCiState st{ stop = cmd }

setPrompt value = do
  st <- getGHCiState
  if null value
      then liftIO $ hPutStrLn stderr $ "syntax: :set prompt <prompt>, currently \"" ++ prompt st ++ "\""
      else case value of
           '\"' : _ -> case reads value of
                       [(value', xs)] | all isSpace xs ->
                           setGHCiState (st { prompt = value' })
                       _ ->
                           liftIO $ hPutStrLn stderr "Can't parse prompt string. Use Haskell syntax."
           _ -> setGHCiState (st { prompt = value })

setOptions wds =
   do
      let (plus_opts, minus_opts)  = partitionWith isPlus wds
      mapM_ setOpt plus_opts
      newDynFlags minus_opts

newDynFlags :: [String] -> GHCi ()
newDynFlags minus_opts = do
      dflags <- getDynFlags
      let pkg_flags = packageFlags dflags
      (dflags', leftovers, warns) <- liftIO $ GHC.parseDynamicFlags dflags $ map noLoc minus_opts
      liftIO $ handleFlagWarnings dflags' warns

      if (not (null leftovers))
        then ghcError . CmdLineError
           $ "Some flags have not been recognized: "
          ++ (concat . intersperse ", " $ map unLoc leftovers)
        else return ()

      new_pkgs <- setDynFlags dflags'

      dflags <- getDynFlags
      when (packageFlags dflags /= pkg_flags) $ do
        liftIO $ hPutStrLn stderr "package flags have changed, resetting and loading new packages..."
        GHC.setTargets []
        _ <- GHC.load LoadAllTargets
        liftIO (linkPackages dflags new_pkgs)
        setContextAfterLoad ([],[]) False []
      return ()

unsetOptions :: String -> GHCi ()
unsetOptions str
  =
     let opts = words str
         (minus_opts, rest1) = partition isMinus opts
         (plus_opts, rest2)  = partitionWith isPlus rest1
         (other_opts, rest3) = partition (`elem` map fst defaulters) rest2

         defaulters = 
           [ ("args"  , setArgs default_args)
           , ("prog"  , setProg default_progname)
           , ("prompt", setPrompt default_prompt)
           , ("editor", liftIO findEditor >>= setEditor)
           , ("stop"  , setStop default_stop)
           ]

         no_flag ('-':'f':rest) = return ("-fno-" ++ rest)
         no_flag f = ghcError (ProgramError ("don't know how to reverse " ++ f))

     in if (not (null rest3))
           then liftIO (putStrLn ("unknown option: '" ++ head rest3 ++ "'"))
           else do
             mapM_ (fromJust.flip lookup defaulters) other_opts

             mapM_ unsetOpt plus_opts

             no_flags <- mapM no_flag minus_opts
             newDynFlags no_flags

isMinus :: String -> Bool
isMinus ('-':_) = True
isMinus _ = False

isPlus :: String -> Either String String
isPlus ('+':opt) = Left opt
isPlus other     = Right other

setOpt, unsetOpt :: String -> GHCi ()

setOpt str
  = case strToGHCiOpt str of
	Nothing -> liftIO (putStrLn ("unknown option: '" ++ str ++ "'"))
	Just o  -> setOption o

unsetOpt str
  = case strToGHCiOpt str of
	Nothing -> liftIO (putStrLn ("unknown option: '" ++ str ++ "'"))
	Just o  -> unsetOption o

strToGHCiOpt :: String -> (Maybe GHCiOption)
strToGHCiOpt "m" = Just Multiline
strToGHCiOpt "s" = Just ShowTiming
strToGHCiOpt "t" = Just ShowType
strToGHCiOpt "r" = Just RevertCAFs
strToGHCiOpt _   = Nothing

optToStr :: GHCiOption -> String
optToStr Multiline  = "m"
optToStr ShowTiming = "s"
optToStr ShowType   = "t"
optToStr RevertCAFs = "r"

showCmd :: String -> GHCi ()
showCmd str = do
  st <- getGHCiState
  case words str of
        ["args"]     -> liftIO $ putStrLn (show (args st))
        ["prog"]     -> liftIO $ putStrLn (show (progname st))
        ["prompt"]   -> liftIO $ putStrLn (show (prompt st))
        ["editor"]   -> liftIO $ putStrLn (show (editor st))
        ["stop"]     -> liftIO $ putStrLn (show (stop st))
	["modules" ] -> showModules
	["bindings"] -> showBindings
	["linker"]   -> liftIO showLinkerState
        ["breaks"]   -> showBkptTable
        ["context"]  -> showContext
        ["packages"]  -> showPackages
        ["languages"]  -> showLanguages
	_ -> ghcError (CmdLineError ("syntax:  :show [ args | prog | prompt | editor | stop | modules | bindings\n"++
                                     "               | breaks | context | packages | languages ]"))

showModules :: GHCi ()
showModules = do
  loaded_mods <- getLoadedModules
  let show_one ms = do m <- GHC.showModule ms; liftIO (putStrLn m)
  mapM_ show_one loaded_mods

getLoadedModules :: GHC.GhcMonad m => m [GHC.ModSummary]
getLoadedModules = do
  graph <- GHC.getModuleGraph
  filterM (GHC.isLoaded . GHC.ms_mod_name) graph

showBindings :: GHCi ()
showBindings = do
  bindings <- GHC.getBindings
  docs     <- pprTypeAndContents
                  [ id | AnId id <- sortBy compareTyThings bindings]
  printForUserPartWay docs

compareTyThings :: TyThing -> TyThing -> Ordering
t1 `compareTyThings` t2 = getName t1 `compareNames` getName t2

printTyThing :: TyThing -> GHCi ()
printTyThing tyth = do dflags <- getDynFlags
                       let pefas = dopt Opt_PrintExplicitForalls dflags
		       printForUser (pprTyThing pefas tyth)

showBkptTable :: GHCi ()
showBkptTable = do
  st <- getGHCiState
  printForUser $ prettyLocations (breaks st)

showContext :: GHCi ()
showContext = do
   resumes <- GHC.getResumeContext
   printForUser $ vcat (map pp_resume (reverse resumes))
  where
   pp_resume resume =
        ptext (sLit "--> ") <> text (GHC.resumeStmt resume)
        $$ nest 2 (ptext (sLit "Stopped at") <+> ppr (GHC.resumeSpan resume))

showPackages :: GHCi ()
showPackages = do
  pkg_flags <- fmap packageFlags getDynFlags
  liftIO $ putStrLn $ showSDoc $ vcat $
    text ("active package flags:"++if null pkg_flags then " none" else "")
    : map showFlag pkg_flags
  where showFlag (ExposePackage p) = text $ "  -package " ++ p
        showFlag (HidePackage p)   = text $ "  -hide-package " ++ p
        showFlag (IgnorePackage p) = text $ "  -ignore-package " ++ p
        showFlag (ExposePackageId p) = text $ "  -package-id " ++ p

showLanguages :: GHCi ()
showLanguages = do
   dflags <- getDynFlags
   liftIO $ putStrLn $ showSDoc $ vcat $
      text "active language flags:" :
      [text ("  -X" ++ str) | (str, f, _) <- DynFlags.xFlags, xopt f dflags]

completeCmd, completeMacro, completeIdentifier, completeModule,
    completeSetModule,
    completeHomeModule, completeSetOptions, completeShowOptions,
    completeHomeModuleOrFile, completeExpression
    :: CompletionFunc GHCi

ghciCompleteWord :: CompletionFunc GHCi
ghciCompleteWord line@(left,_) = case firstWord of
    ':':cmd     | null rest     -> completeCmd line
                | otherwise     -> do
                        completion <- lookupCompletion cmd
                        completion line
    "import"    -> completeModule line
    _           -> completeExpression line
  where
    (firstWord,rest) = break isSpace $ dropWhile isSpace $ reverse left
    lookupCompletion ('!':_) = return completeFilename
    lookupCompletion c = do
        maybe_cmd <- liftIO $ lookupCommand' c
        case maybe_cmd of
            Just (_,_,f) -> return f
            Nothing -> return completeFilename

completeCmd = wrapCompleter " " $ \w -> do
  macros <- liftIO $ readIORef macros_ref
  let macro_names = map (':':) . map cmdName $ macros
  let command_names = map (':':) . map cmdName $ builtin_commands
  let{ candidates = case w of
      ':' : ':' : _ -> map (':':) command_names
      _ -> nub $ macro_names ++ command_names }
  return $ filter (w `isPrefixOf`) candidates

completeMacro = wrapIdentCompleter $ \w -> do
  cmds <- liftIO $ readIORef macros_ref
  return (filter (w `isPrefixOf`) (map cmdName cmds))

completeIdentifier = wrapIdentCompleter $ \w -> do
  rdrs <- GHC.getRdrNamesInScope
  return (filter (w `isPrefixOf`) (map (showSDoc.ppr) rdrs))

completeModule = wrapIdentCompleter $ \w -> do
  dflags <- GHC.getSessionDynFlags
  let pkg_mods = allExposedModules dflags
  loaded_mods <- liftM (map GHC.ms_mod_name) getLoadedModules
  return $ filter (w `isPrefixOf`)
        $ map (showSDoc.ppr) $ loaded_mods ++ pkg_mods

completeSetModule = wrapIdentCompleterWithModifier "+-" $ \m w -> do
  modules <- case m of
    Just '-' -> do
      (toplevs, exports) <- GHC.getContext
      return $ map GHC.moduleName (nub (map fst exports) ++ toplevs)
    _ -> do
      dflags <- GHC.getSessionDynFlags
      let pkg_mods = allExposedModules dflags
      loaded_mods <- liftM (map GHC.ms_mod_name) getLoadedModules
      return $ loaded_mods ++ pkg_mods
  return $ filter (w `isPrefixOf`) $ map (showSDoc.ppr) modules

completeHomeModule = wrapIdentCompleter listHomeModules

listHomeModules :: String -> GHCi [String]
listHomeModules w = do
    g <- GHC.getModuleGraph
    let home_mods = map GHC.ms_mod_name g
    return $ sort $ filter (w `isPrefixOf`)
            $ map (showSDoc.ppr) home_mods

completeSetOptions = wrapCompleter flagWordBreakChars $ \w -> do
  return (filter (w `isPrefixOf`) options)
    where options = "args":"prog":"prompt":"editor":"stop":flagList
          flagList = map head $ group $ sort allFlags

completeShowOptions = wrapCompleter flagWordBreakChars $ \w -> do
  return (filter (w `isPrefixOf`) options)
    where options = ["args", "prog", "prompt", "editor", "stop",
                     "modules", "bindings", "linker", "breaks",
                     "context", "packages", "languages"]

completeHomeModuleOrFile = completeWord Nothing filenameWordBreakChars
                $ unionComplete (fmap (map simpleCompletion) . listHomeModules)
                            listFiles

unionComplete :: Monad m => (a -> m [b]) -> (a -> m [b]) -> a -> m [b]
unionComplete f1 f2 line = do
  cs1 <- f1 line
  cs2 <- f2 line
  return (cs1 ++ cs2)

wrapCompleter :: String -> (String -> GHCi [String]) -> CompletionFunc GHCi
wrapCompleter breakChars fun = completeWord Nothing breakChars
    $ fmap (map simpleCompletion) . fmap sort . fun

wrapIdentCompleter :: (String -> GHCi [String]) -> CompletionFunc GHCi
wrapIdentCompleter = wrapCompleter word_break_chars

wrapIdentCompleterWithModifier :: String -> (Maybe Char -> String -> GHCi [String]) -> CompletionFunc GHCi
wrapIdentCompleterWithModifier modifChars fun = completeWordWithPrev Nothing word_break_chars
    $ \rest -> fmap (map simpleCompletion) . fmap sort . fun (getModifier rest)
 where
  getModifier = find (`elem` modifChars)

allExposedModules :: DynFlags -> [ModuleName]
allExposedModules dflags 
 = concat (map exposedModules (filter exposed (eltsUFM pkg_db)))
 where
  pkg_db = pkgIdMap (pkgState dflags)

completeExpression = completeQuotedWord (Just '\\') "\"" listFiles
                        completeIdentifier

handler :: SomeException -> GHCi Bool

handler exception = do
  flushInterpBuffers
  liftIO installSignalHandlers
  ghciHandle handler (showException exception >> return False)

showException :: SomeException -> GHCi ()
showException se =
  liftIO $ case fromException se of
           Just (CmdLineError s)    -> putStrLn s
           Just ph@(PhaseFailed {}) -> putStrLn (showGhcException ph "")
           Just other_ghc_ex        -> print other_ghc_ex
           Nothing                  ->
               case fromException se of
               Just UserInterrupt -> putStrLn "Interrupted."
               _                  -> putStrLn ("*** Exception: " ++ show se)

ghciHandle :: MonadException m => (SomeException -> m a) -> m a -> m a
ghciHandle h m = Haskeline.catch m $ \e -> unblock (h e)

ghciTry :: GHCi a -> GHCi (Either SomeException a)
ghciTry (GHCi m) = GHCi $ \s -> gtry (m s)

expandPath :: MonadIO m => String -> InputT m String
expandPath path = do
    exp_path <- liftIO $ expandPathIO path
    enc <- fmap BS.unpack $ Encoding.encode exp_path
    return enc

expandPathIO :: String -> IO String
expandPathIO path = 
  case dropWhile isSpace path of
   ('~':d) -> do
	tilde <- getHomeDirectory
	return (tilde ++ '/':d)
   other -> 
	return other

wantInterpretedModule :: GHC.GhcMonad m => String -> m Module
wantInterpretedModule str = do
   modl <- lookupModule str
   dflags <- getDynFlags
   when (GHC.modulePackageId modl /= thisPackage dflags) $
      ghcError (CmdLineError ("module '" ++ str ++ "' is from another package;\nthis command requires an interpreted module"))
   is_interpreted <- GHC.moduleIsInterpreted modl
   when (not is_interpreted) $
       ghcError (CmdLineError ("module '" ++ str ++ "' is not interpreted; try \':add *" ++ str ++ "' first"))
   return modl

wantNameFromInterpretedModule :: GHC.GhcMonad m
                              => (Name -> SDoc -> m ())
                              -> String
                              -> (Name -> m ())
                              -> m ()
wantNameFromInterpretedModule noCanDo str and_then =
  handleSourceError GHC.printException $ do
   names <- GHC.parseName str
   case names of
      []    -> return ()
      (n:_) -> do
            let modl = ASSERT( isExternalName n ) GHC.nameModule n
            if not (GHC.isExternalName n)
               then noCanDo n $ ppr n <>
                                text " is not defined in an interpreted module"
               else do
            is_interpreted <- GHC.moduleIsInterpreted modl
            if not is_interpreted
               then noCanDo n $ text "module " <> ppr modl <>
                                text " is not interpreted"
               else and_then n

sprintCmd, printCmd, forceCmd :: String -> GHCi ()
sprintCmd = pprintCommand False False
printCmd  = pprintCommand True False
forceCmd  = pprintCommand False True

pprintCommand :: Bool -> Bool -> String -> GHCi ()
pprintCommand bind force str = do
  pprintClosureCommand bind force str

stepCmd :: String -> GHCi ()
stepCmd []         = doContinue (const True) GHC.SingleStep
stepCmd expression = runStmt expression GHC.SingleStep >> return ()

stepLocalCmd :: String -> GHCi ()
stepLocalCmd  [] = do 
  mb_span <- getCurrentBreakSpan
  case mb_span of
    Nothing  -> stepCmd []
    Just loc -> do
       Just mod <- getCurrentBreakModule
       current_toplevel_decl <- enclosingTickSpan mod loc
       doContinue (`isSubspanOf` current_toplevel_decl) GHC.SingleStep

stepLocalCmd expression = stepCmd expression

stepModuleCmd :: String -> GHCi ()
stepModuleCmd  [] = do 
  mb_span <- getCurrentBreakSpan
  case mb_span of
    Nothing  -> stepCmd []
    Just _ -> do
       Just span <- getCurrentBreakSpan
       let f some_span = srcSpanFileName_maybe span == srcSpanFileName_maybe some_span
       doContinue f GHC.SingleStep

stepModuleCmd expression = stepCmd expression

enclosingTickSpan :: Module -> SrcSpan -> GHCi SrcSpan
enclosingTickSpan mod src = do
  ticks <- getTickArray mod
  let line = srcSpanStartLine src
  ASSERT (inRange (bounds ticks) line) do
  let enclosing_spans = [ span | (_,span) <- ticks ! line
                               , srcSpanEnd span >= srcSpanEnd src]
  return . head . sortBy leftmost_largest $ enclosing_spans

traceCmd :: String -> GHCi ()
traceCmd []         = doContinue (const True) GHC.RunAndLogSteps
traceCmd expression = runStmt expression GHC.RunAndLogSteps >> return ()

continueCmd :: String -> GHCi ()
continueCmd = noArgs $ doContinue (const True) GHC.RunToCompletion

doContinue :: (SrcSpan -> Bool) -> SingleStep -> GHCi ()
doContinue pred step = do 
  runResult <- resume pred step
  _ <- afterRunStmt pred runResult
  return ()

abandonCmd :: String -> GHCi ()
abandonCmd = noArgs $ do
  b <- GHC.abandon
  when (not b) $ liftIO $ putStrLn "There is no computation running."

deleteCmd :: String -> GHCi ()
deleteCmd argLine = do
   deleteSwitch $ words argLine
   where
   deleteSwitch :: [String] -> GHCi ()
   deleteSwitch [] =
      liftIO $ putStrLn "The delete command requires at least one argument."
   deleteSwitch ("*":_rest) = discardActiveBreakPoints
   deleteSwitch idents = do
      mapM_ deleteOneBreak idents 
      where
      deleteOneBreak :: String -> GHCi ()
      deleteOneBreak str
         | all isDigit str = deleteBreak (read str)
         | otherwise = return ()

historyCmd :: String -> GHCi ()
historyCmd arg
  | null arg        = history 20
  | all isDigit arg = history (read arg)
  | otherwise       = liftIO $ putStrLn "Syntax:  :history [num]"
  where
  history num = do
    resumes <- GHC.getResumeContext
    case resumes of
      [] -> liftIO $ putStrLn "Not stopped at a breakpoint"
      (r:_) -> do
        let hist = GHC.resumeHistory r
            (took,rest) = splitAt num hist
        case hist of
          [] -> liftIO $ putStrLn $
                   "Empty history. Perhaps you forgot to use :trace?"
          _  -> do
                 spans <- mapM GHC.getHistorySpan took
                 let nums  = map (printf "-%-3d:") [(1::Int)..]
                     names = map GHC.historyEnclosingDecls took
                 printForUser (vcat(zipWith3 
                                 (\x y z -> x <+> y <+> z) 
                                 (map text nums) 
                                 (map (bold . hcat . punctuate colon . map text) names)
                                 (map (parens . ppr) spans)))
                 liftIO $ putStrLn $ if null rest then "<end of history>" else "..."

bold :: SDoc -> SDoc
bold c | do_bold   = text start_bold <> c <> text end_bold
       | otherwise = c

backCmd :: String -> GHCi ()
backCmd = noArgs $ do
  (names, _, span) <- GHC.back
  printForUser $ ptext (sLit "Logged breakpoint at") <+> ppr span
  printTypeOfNames names
  st <- getGHCiState
  enqueueCommands [stop st]

forwardCmd :: String -> GHCi ()
forwardCmd = noArgs $ do
  (names, ix, span) <- GHC.forward
  printForUser $ (if (ix == 0)
                    then ptext (sLit "Stopped at")
                    else ptext (sLit "Logged breakpoint at")) <+> ppr span
  printTypeOfNames names
  st <- getGHCiState
  enqueueCommands [stop st]

breakCmd :: String -> GHCi ()
breakCmd argLine = do
   breakSwitch $ words argLine

breakSwitch :: [String] -> GHCi ()
breakSwitch [] = do
   liftIO $ putStrLn "The break command requires at least one argument."
breakSwitch (arg1:rest)
   | looksLikeModuleName arg1 && not (null rest) = do
        mod <- wantInterpretedModule arg1
        breakByModule mod rest
   | all isDigit arg1 = do
        (toplevel, _) <- GHC.getContext
        case toplevel of
           (mod : _) -> breakByModuleLine mod (read arg1) rest
           [] -> do 
              liftIO $ putStrLn "Cannot find default module for breakpoint." 
              liftIO $ putStrLn "Perhaps no modules are loaded for debugging?"
   | otherwise = do
        wantNameFromInterpretedModule noCanDo arg1 $ \name -> do
        let loc = GHC.srcSpanStart (GHC.nameSrcSpan name)
        if GHC.isGoodSrcLoc loc
               then ASSERT( isExternalName name ) 
	       	    findBreakAndSet (GHC.nameModule name) $ 
                         findBreakByCoord (Just (GHC.srcLocFile loc))
                                          (GHC.srcLocLine loc, 
                                           GHC.srcLocCol loc)
               else noCanDo name $ text "can't find its location: " <> ppr loc
       where
          noCanDo n why = printForUser $
                text "cannot set breakpoint on " <> ppr n <> text ": " <> why

breakByModule :: Module -> [String] -> GHCi () 
breakByModule mod (arg1:rest)
   | all isDigit arg1 = do
        breakByModuleLine mod (read arg1) rest
breakByModule _ _
   = breakSyntax

breakByModuleLine :: Module -> Int -> [String] -> GHCi ()
breakByModuleLine mod line args
   | [] <- args = findBreakAndSet mod $ findBreakByLine line
   | [col] <- args, all isDigit col =
        findBreakAndSet mod $ findBreakByCoord Nothing (line, read col)
   | otherwise = breakSyntax

breakSyntax :: a
breakSyntax = ghcError (CmdLineError "Syntax: :break [<mod>] <line> [<column>]")

findBreakAndSet :: Module -> (TickArray -> Maybe (Int, SrcSpan)) -> GHCi ()
findBreakAndSet mod lookupTickTree = do 
   tickArray <- getTickArray mod
   (breakArray, _) <- getModBreak mod
   case lookupTickTree tickArray of 
      Nothing  -> liftIO $ putStrLn $ "No breakpoints found at that location."
      Just (tick, span) -> do
         success <- liftIO $ setBreakFlag True breakArray tick
         if success 
            then do
               (alreadySet, nm) <- 
                     recordBreak $ BreakLocation
                             { breakModule = mod
                             , breakLoc = span
                             , breakTick = tick
                             , onBreakCmd = ""
                             }
               printForUser $
                  text "Breakpoint " <> ppr nm <>
                  if alreadySet 
                     then text " was already set at " <> ppr span
                     else text " activated at " <> ppr span
            else do
            printForUser $ text "Breakpoint could not be activated at" 
                                 <+> ppr span

findBreakByLine :: Int -> TickArray -> Maybe (BreakIndex,SrcSpan)
findBreakByLine line arr
  | not (inRange (bounds arr) line) = Nothing
  | otherwise =
    listToMaybe (sortBy (leftmost_largest `on` snd)  complete)   `mplus`
    listToMaybe (sortBy (leftmost_smallest `on` snd) incomplete) `mplus`
    listToMaybe (sortBy (rightmost `on` snd) ticks)
  where 
        ticks = arr ! line

        starts_here = [ tick | tick@(_,span) <- ticks,
                               GHC.srcSpanStartLine span == line ]

        (complete,incomplete) = partition ends_here starts_here
            where ends_here (_,span) = GHC.srcSpanEndLine span == line

findBreakByCoord :: Maybe FastString -> (Int,Int) -> TickArray
                 -> Maybe (BreakIndex,SrcSpan)
findBreakByCoord mb_file (line, col) arr
  | not (inRange (bounds arr) line) = Nothing
  | otherwise =
    listToMaybe (sortBy (rightmost `on` snd) contains ++
                 sortBy (leftmost_smallest `on` snd) after_here)
  where 
        ticks = arr ! line

        contains = [ tick | tick@(_,span) <- ticks, span `spans` (line,col),
                            is_correct_file span ]

        is_correct_file span
                 | Just f <- mb_file = GHC.srcSpanFile span == f
                 | otherwise         = True

        after_here = [ tick | tick@(_,span) <- ticks,
                              GHC.srcSpanStartLine span == line,
                              GHC.srcSpanStartCol span >= col ]

do_bold :: Bool
do_bold = (`isPrefixOf` unsafePerformIO mTerm) `any` ["xterm", "linux"]
    where mTerm = System.Environment.getEnv "TERM"
                  `catchIO` \_ -> return "TERM not set"

start_bold :: String
start_bold = "\ESC[1m"
end_bold :: String
end_bold   = "\ESC[0m"

listCmd :: String -> InputT GHCi ()
listCmd c = listCmd' c

listCmd' :: String -> InputT GHCi ()
listCmd' "" = do
   mb_span <- lift getCurrentBreakSpan
   case mb_span of
      Nothing ->
          printForUser $ text "Not stopped at a breakpoint; nothing to list"
      Just span
       | GHC.isGoodSrcSpan span -> listAround span True
       | otherwise ->
          do resumes <- GHC.getResumeContext
             case resumes of
                 [] -> panic "No resumes"
                 (r:_) ->
                     do let traceIt = case GHC.resumeHistory r of
                                      [] -> text "rerunning with :trace,"
                                      _ -> empty
                            doWhat = traceIt <+> text ":back then :list"
                        printForUser (text "Unable to list source for" <+>
                                      ppr span
                                   $$ text "Try" <+> doWhat)
listCmd' str = list2 (words str)

list2 :: [String] -> InputT GHCi ()
list2 [arg] | all isDigit arg = do
    (toplevel, _) <- GHC.getContext
    case toplevel of
        [] -> liftIO $ putStrLn "No module to list"
        (mod : _) -> listModuleLine mod (read arg)
list2 [arg1,arg2] | looksLikeModuleName arg1, all isDigit arg2 = do
        mod <- wantInterpretedModule arg1
        listModuleLine mod (read arg2)
list2 [arg] = do
        wantNameFromInterpretedModule noCanDo arg $ \name -> do
        let loc = GHC.srcSpanStart (GHC.nameSrcSpan name)
        if GHC.isGoodSrcLoc loc
               then do
                  tickArray <- ASSERT( isExternalName name )
		  	       lift $ getTickArray (GHC.nameModule name)
                  let mb_span = findBreakByCoord (Just (GHC.srcLocFile loc))
                                        (GHC.srcLocLine loc, GHC.srcLocCol loc)
                                        tickArray
                  case mb_span of
                    Nothing       -> listAround (GHC.srcLocSpan loc) False
                    Just (_,span) -> listAround span False
               else
                  noCanDo name $ text "can't find its location: " <>
                                 ppr loc
    where
        noCanDo n why = printForUser $
            text "cannot list source code for " <> ppr n <> text ": " <> why
list2  _other = 
        liftIO $ putStrLn "syntax:  :list [<line> | <module> <line> | <identifier>]"

listModuleLine :: Module -> Int -> InputT GHCi ()
listModuleLine modl line = do
   graph <- GHC.getModuleGraph
   let this = filter ((== modl) . GHC.ms_mod) graph
   case this of
     [] -> panic "listModuleLine"
     summ:_ -> do
           let filename = expectJust "listModuleLine" (ml_hs_file (GHC.ms_location summ))
               loc = GHC.mkSrcLoc (mkFastString (filename)) line 0
           listAround (GHC.srcLocSpan loc) False

listAround :: MonadIO m => SrcSpan -> Bool -> InputT m ()
listAround span do_highlight = do
      contents <- liftIO $ BS.readFile (unpackFS file)
      let 
          lines = BS.split '\n' contents
          these_lines = take (line2 - line1 + 1 + pad_before + pad_after) $ 
                        drop (line1 - 1 - pad_before) $ lines
          fst_line = max 1 (line1 - pad_before)
          line_nos = [ fst_line .. ]

          highlighted | do_highlight = zipWith highlight line_nos these_lines
                      | otherwise    = [\p -> BS.concat[p,l] | l <- these_lines]

          bs_line_nos = [ BS.pack (show l ++ "  ") | l <- line_nos ]
          prefixed = zipWith ($) highlighted bs_line_nos
      let output = BS.intercalate (BS.pack "\n") prefixed
      utf8Decoded <- liftIO $ BS.useAsCStringLen output
                        $ \(p,n) -> utf8DecodeString (castPtr p) n
      liftIO $ putStrLn utf8Decoded
  where
        file  = GHC.srcSpanFile span
        line1 = GHC.srcSpanStartLine span
        col1  = GHC.srcSpanStartCol span - 1
        line2 = GHC.srcSpanEndLine span
        col2  = GHC.srcSpanEndCol span - 1

        pad_before | line1 == 1 = 0
                   | otherwise  = 1
        pad_after = 1

        highlight | do_bold   = highlight_bold
                  | otherwise = highlight_carets

        highlight_bold no line prefix
          | no == line1 && no == line2
          = let (a,r) = BS.splitAt col1 line
                (b,c) = BS.splitAt (col2-col1) r
            in
            BS.concat [prefix, a,BS.pack start_bold,b,BS.pack end_bold,c]
          | no == line1
          = let (a,b) = BS.splitAt col1 line in
            BS.concat [prefix, a, BS.pack start_bold, b]
          | no == line2
          = let (a,b) = BS.splitAt col2 line in
            BS.concat [prefix, a, BS.pack end_bold, b]
          | otherwise   = BS.concat [prefix, line]

        highlight_carets no line prefix
          | no == line1 && no == line2
          = BS.concat [prefix, line, nl, indent, BS.replicate col1 ' ',
                                         BS.replicate (col2-col1) '^']
          | no == line1
          = BS.concat [indent, BS.replicate (col1 - 2) ' ', BS.pack "vv", nl, 
                                         prefix, line]
          | no == line2
          = BS.concat [prefix, line, nl, indent, BS.replicate col2 ' ',
                                         BS.pack "^^"]
          | otherwise   = BS.concat [prefix, line]
         where
           indent = BS.pack ("  " ++ replicate (length (show no)) ' ')
           nl = BS.singleton '\n'

getTickArray :: Module -> GHCi TickArray
getTickArray modl = do
   st <- getGHCiState
   let arrmap = tickarrays st
   case lookupModuleEnv arrmap modl of
      Just arr -> return arr
      Nothing  -> do
        (_breakArray, ticks) <- getModBreak modl 
        let arr = mkTickArray (assocs ticks)
        setGHCiState st{tickarrays = extendModuleEnv arrmap modl arr}
        return arr

discardTickArrays :: GHCi ()
discardTickArrays = do
   st <- getGHCiState
   setGHCiState st{tickarrays = emptyModuleEnv}

mkTickArray :: [(BreakIndex,SrcSpan)] -> TickArray
mkTickArray ticks
  = accumArray (flip (:)) [] (1, max_line) 
        [ (line, (nm,span)) | (nm,span) <- ticks,
                              line <- srcSpanLines span ]
    where
        max_line = foldr max 0 (map GHC.srcSpanEndLine (map snd ticks))
        srcSpanLines span = [ GHC.srcSpanStartLine span .. 
                              GHC.srcSpanEndLine span ]

lookupModule :: GHC.GhcMonad m => String -> m Module
lookupModule modName
   = GHC.lookupModule (GHC.mkModuleName modName) Nothing

discardActiveBreakPoints :: GHCi ()
discardActiveBreakPoints = do
   st <- getGHCiState
   mapM_ (turnOffBreak.snd) (breaks st)
   setGHCiState $ st { breaks = [] }

deleteBreak :: Int -> GHCi ()
deleteBreak identity = do
   st <- getGHCiState
   let oldLocations    = breaks st
       (this,rest)     = partition (\loc -> fst loc == identity) oldLocations
   if null this 
      then printForUser (text "Breakpoint" <+> ppr identity <+>
                         text "does not exist")
      else do
           mapM_ (turnOffBreak.snd) this
           setGHCiState $ st { breaks = rest }

turnOffBreak :: BreakLocation -> GHCi Bool
turnOffBreak loc = do
  (arr, _) <- getModBreak (breakModule loc)
  liftIO $ setBreakFlag False arr (breakTick loc)

getModBreak :: Module -> GHCi (GHC.BreakArray, Array Int SrcSpan)
getModBreak mod = do
   Just mod_info <- GHC.getModuleInfo mod
   let modBreaks  = GHC.modInfoModBreaks mod_info
   let array      = GHC.modBreaks_flags modBreaks
   let ticks      = GHC.modBreaks_locs  modBreaks
   return (array, ticks)

setBreakFlag :: Bool -> GHC.BreakArray -> Int -> IO Bool 
setBreakFlag toggle array index
   | toggle    = GHC.setBreakOn array index 
   | otherwise = GHC.setBreakOff array index
