-- <prana>
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- </prana>
{-# LANGUAGE CPP, NondecreasingIndentation, TupleSections #-}
{-# OPTIONS -optc-DNON_POSIX_SOURCE #-}

-----------------------------------------------------------------------------
--
-- GHC Driver program
--
-- (c) The University of Glasgow 2005
--
-----------------------------------------------------------------------------

module Main  where

-- <prana>
import qualified Data.ByteString as S
import Control.Exception
import Data.Bits
import Data.ByteString (ByteString)
import Data.Foldable (foldlM)
import Data.IORef
import Data.Int
import qualified DataCon as GHC
import Data.List (unfoldr)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Proxy
import Data.Word
import Foreign (ForeignPtr, Ptr, Storable(..), plusPtr, minusPtr, castPtr,
                withForeignPtr, mallocBytes, free, allocaBytes)
import GHC.Base (unsafeChr, ord)
import GHC.Generics
import GHC.Real (Ratio(..))
import GHC.TypeLits
import Numeric.Natural
import System.IO.Unsafe
import qualified Control.Monad.Fail as Fail
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Monoid as M
import qualified Data.ByteString.Char8 as S8
import Data.Semigroup
import Data.Word (Word8)
import qualified Outputable as GHC
import qualified HscTypes as GHC
import qualified Literal as GHC
import qualified Id as GHC
import qualified Module as GHC
import qualified CoreSyn
import qualified Name as GHC
-- </prana>

-- The official GHC API
import qualified GHC
import GHC              ( -- DynFlags(..), HscTarget(..),
                          -- GhcMode(..), GhcLink(..),
                          Ghc, GhcMonad(..),
                          LoadHowMuch(..) )
import CmdLineParser

-- Implementations of the various modes (--show-iface, mkdependHS. etc.)
import LoadIface        ( showIface )
import HscMain          ( newHscEnv )
import DriverPipeline   ( oneShot, compileFile )
import DriverMkDepend   ( doMkDependHS )
#ifdef GHCI
import GHCi.UI          ( interactiveUI, ghciWelcomeMsg, defaultGhciSettings )
#endif

-- Frontend plugins
#ifdef GHCI
import DynamicLoading
import Plugins
#else
import DynamicLoading   ( pluginError )
#endif
import Module           ( ModuleName )


-- Various other random stuff that we need
import Config
import Constants
import HscTypes
import Packages         ( pprPackages, pprPackagesSimple, pprModuleMap )
import DriverPhases
import BasicTypes       ( failed )
import StaticFlags
import DynFlags
import ErrUtils
import FastString
import Outputable hiding ((<>))
import qualified Outputable ((<>))
import SrcLoc
import Util
import Panic
import UniqSupply
import MonadUtils       ( liftIO )

-- Imports for --abi-hash
import LoadIface           ( loadUserInterface )
import Module              ( mkModuleName )
import Finder              ( findImportedModule, cannotFindInterface )
import TcRnMonad           ( initIfaceCheck )
import Binary              ( openBinMem, put_, fingerprintBinMem )

-- Standard Haskell libraries
import System.IO
import System.Environment
import System.Exit
import System.FilePath
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe

-----------------------------------------------------------------------------
-- ToDo:

-- time commands when run with -v
-- user ways
-- Win32 support: proper signal handling
-- reading the package configuration file is too slow
-- -K<size>

-----------------------------------------------------------------------------
-- GHC's command-line interface

main :: IO ()
main = do
   initGCStatistics -- See Note [-Bsymbolic and hooks]
   hSetBuffering stdout LineBuffering
   hSetBuffering stderr LineBuffering

   -- Handle GHC-specific character encoding flags, allowing us to control how
   -- GHC produces output regardless of OS.
   env <- getEnvironment
   case lookup "GHC_CHARENC" env of
    Just "UTF-8" -> do
     hSetEncoding stdout utf8
     hSetEncoding stderr utf8
    _ -> do
     -- Avoid GHC erroring out when trying to display unhandled characters
     hSetTranslit stdout
     hSetTranslit stderr

   GHC.defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    -- 1. extract the -B flag from the args
    argv0 <- getArgs

    let (minusB_args, argv1) = partition ("-B" `isPrefixOf`) argv0
        mbMinusB | null minusB_args = Nothing
                 | otherwise = Just (drop 2 (last minusB_args))

    let argv1' = map (mkGeneralLocated "on the commandline") argv1
    (argv2, staticFlagWarnings) <- parseStaticFlags argv1'

    -- 2. Parse the "mode" flags (--make, --interactive etc.)
    (mode, argv3, modeFlagWarnings) <- parseModeFlags argv2

    let flagWarnings = staticFlagWarnings ++ modeFlagWarnings

    -- If all we want to do is something like showing the version number
    -- then do it now, before we start a GHC session etc. This makes
    -- getting basic information much more resilient.

    -- In particular, if we wait until later before giving the version
    -- number then bootstrapping gets confused, as it tries to find out
    -- what version of GHC it's using before package.conf exists, so
    -- starting the session fails.
    case mode of
        Left preStartupMode ->
            do case preStartupMode of
                   ShowSupportedExtensions   -> showSupportedExtensions
                   ShowVersion               -> showVersion
                   ShowNumVersion            -> putStrLn cProjectVersion
                   ShowOptions isInteractive -> showOptions isInteractive
        Right postStartupMode ->
            -- start our GHC session
            GHC.runGhc mbMinusB $ do

            dflags <- GHC.getSessionDynFlags

            case postStartupMode of
                Left preLoadMode ->
                    liftIO $ do
                        case preLoadMode of
                            ShowInfo               -> showInfo dflags
                            ShowGhcUsage           -> showGhcUsage  dflags
                            ShowGhciUsage          -> showGhciUsage dflags
                            PrintWithDynFlags f    -> putStrLn (f dflags)
                Right postLoadMode ->
                    main' postLoadMode dflags argv3 flagWarnings

main' :: PostLoadMode -> DynFlags -> [Located String] -> [Located String]
      -> Ghc ()
main' postLoadMode dflags0 args flagWarnings = do
  -- set the default GhcMode, HscTarget and GhcLink.  The HscTarget
  -- can be further adjusted on a module by module basis, using only
  -- the -fvia-C and -fasm flags.  If the default HscTarget is not
  -- HscC or HscAsm, -fvia-C and -fasm have no effect.
  let dflt_target = hscTarget dflags0
      (mode, lang, link)
         = case postLoadMode of
               DoInteractive   -> (CompManager, HscInterpreted, LinkInMemory)
               DoEval _        -> (CompManager, HscInterpreted, LinkInMemory)
               DoMake          -> (CompManager, dflt_target,    LinkBinary)
               DoMkDependHS    -> (MkDepend,    dflt_target,    LinkBinary)
               DoAbiHash       -> (OneShot,     dflt_target,    LinkBinary)
               _               -> (OneShot,     dflt_target,    LinkBinary)

  let dflags1 = dflags0{ ghcMode   = mode,
                         hscTarget = lang,
                         ghcLink   = link,
                         verbosity = case postLoadMode of
                                         DoEval _ -> 0
                                         _other   -> 1
                        }

      -- turn on -fimplicit-import-qualified for GHCi now, so that it
      -- can be overriden from the command-line
      -- XXX: this should really be in the interactive DynFlags, but
      -- we don't set that until later in interactiveUI
      dflags2  | DoInteractive <- postLoadMode = imp_qual_enabled
               | DoEval _      <- postLoadMode = imp_qual_enabled
               | otherwise                     = dflags1
        where imp_qual_enabled = dflags1 `gopt_set` Opt_ImplicitImportQualified

        -- The rest of the arguments are "dynamic"
        -- Leftover ones are presumably files
  (dflags3, fileish_args, dynamicFlagWarnings) <-
      GHC.parseDynamicFlags dflags2 args

  let dflags4 = case lang of
                HscInterpreted | not (gopt Opt_ExternalInterpreter dflags3) ->
                    let platform = targetPlatform dflags3
                        dflags3a = updateWays $ dflags3 { ways = interpWays }
                        dflags3b = foldl gopt_set dflags3a
                                 $ concatMap (wayGeneralFlags platform)
                                             interpWays
                        dflags3c = foldl gopt_unset dflags3b
                                 $ concatMap (wayUnsetGeneralFlags platform)
                                             interpWays
                    in dflags3c
                _ ->
                    dflags3

  GHC.prettyPrintGhcErrors dflags4 $ do

  let flagWarnings' = flagWarnings ++ dynamicFlagWarnings

  handleSourceError (\e -> do
       GHC.printException e
       liftIO $ exitWith (ExitFailure 1)) $ do
         liftIO $ handleFlagWarnings dflags4 flagWarnings'

  liftIO $ showBanner postLoadMode dflags4

  let
     -- To simplify the handling of filepaths, we normalise all filepaths right
     -- away - e.g., for win32 platforms, backslashes are converted
     -- into forward slashes.
    normal_fileish_paths = map (normalise . unLoc) fileish_args
    (srcs, objs)         = partition_args normal_fileish_paths [] []

    dflags5 = dflags4 { ldInputs = map (FileOption "") objs
                                   ++ ldInputs dflags4 }

  -- we've finished manipulating the DynFlags, update the session
  _ <- GHC.setSessionDynFlags dflags5
  dflags6 <- GHC.getSessionDynFlags
  hsc_env <- GHC.getSession

        ---------------- Display configuration -----------
  case verbosity dflags6 of
    v | v == 4 -> liftIO $ dumpPackagesSimple dflags6
      | v >= 5 -> liftIO $ dumpPackages dflags6
      | otherwise -> return ()

  when (verbosity dflags6 >= 3) $ do
        liftIO $ hPutStrLn stderr ("Hsc static flags: " ++ unwords staticFlags)


  when (dopt Opt_D_dump_mod_map dflags6) . liftIO $
    printInfoForUser (dflags6 { pprCols = 200 })
                     (pkgQual dflags6) (pprModuleMap dflags6)

  liftIO $ initUniqSupply (initialUnique dflags6) (uniqueIncrement dflags6)
        ---------------- Final sanity checking -----------
  liftIO $ checkOptions postLoadMode dflags6 srcs objs

  ---------------- Do the business -----------
  handleSourceError (\e -> do
       GHC.printException e
       liftIO $ exitWith (ExitFailure 1)) $ do
    case postLoadMode of
       ShowInterface f        -> liftIO $ doShowIface dflags6 f
       DoMake                 -> doMake srcs
       DoMkDependHS           -> doMkDependHS (map fst srcs)
       StopBefore p           -> liftIO (oneShot hsc_env p srcs)
       DoInteractive          -> ghciUI srcs Nothing
       DoEval exprs           -> ghciUI srcs $ Just $ reverse exprs
       DoAbiHash              -> abiHash (map fst srcs)
       ShowPackages           -> liftIO $ showPackages dflags6
       DoFrontend f           -> doFrontend f srcs

  liftIO $ dumpFinalStats dflags6

ghciUI :: [(FilePath, Maybe Phase)] -> Maybe [String] -> Ghc ()
#ifndef GHCI
ghciUI _ _ = throwGhcException (CmdLineError "not built for interactive use")
#else
ghciUI     = interactiveUI defaultGhciSettings
#endif

-- -----------------------------------------------------------------------------
-- Splitting arguments into source files and object files.  This is where we
-- interpret the -x <suffix> option, and attach a (Maybe Phase) to each source
-- file indicating the phase specified by the -x option in force, if any.

partition_args :: [String] -> [(String, Maybe Phase)] -> [String]
               -> ([(String, Maybe Phase)], [String])
partition_args [] srcs objs = (reverse srcs, reverse objs)
partition_args ("-x":suff:args) srcs objs
  | "none" <- suff      = partition_args args srcs objs
  | StopLn <- phase     = partition_args args srcs (slurp ++ objs)
  | otherwise           = partition_args rest (these_srcs ++ srcs) objs
        where phase = startPhase suff
              (slurp,rest) = break (== "-x") args
              these_srcs = zip slurp (repeat (Just phase))
partition_args (arg:args) srcs objs
  | looks_like_an_input arg = partition_args args ((arg,Nothing):srcs) objs
  | otherwise               = partition_args args srcs (arg:objs)

    {-
      We split out the object files (.o, .dll) and add them
      to ldInputs for use by the linker.

      The following things should be considered compilation manager inputs:

       - haskell source files (strings ending in .hs, .lhs or other
         haskellish extension),

       - module names (not forgetting hierarchical module names),

       - things beginning with '-' are flags that were not recognised by
         the flag parser, and we want them to generate errors later in
         checkOptions, so we class them as source files (#5921)

       - and finally we consider everything not containing a '.' to be
         a comp manager input, as shorthand for a .hs or .lhs filename.

      Everything else is considered to be a linker object, and passed
      straight through to the linker.
    -}
looks_like_an_input :: String -> Bool
looks_like_an_input m =  isSourceFilename m
                      || looksLikeModuleName m
                      || "-" `isPrefixOf` m
                      || '.' `notElem` m

-- -----------------------------------------------------------------------------
-- Option sanity checks

-- | Ensure sanity of options.
--
-- Throws 'UsageError' or 'CmdLineError' if not.
checkOptions :: PostLoadMode -> DynFlags -> [(String,Maybe Phase)] -> [String] -> IO ()
     -- Final sanity checking before kicking off a compilation (pipeline).
checkOptions mode dflags srcs objs = do
     -- Complain about any unknown flags
   let unknown_opts = [ f | (f@('-':_), _) <- srcs ]
   when (notNull unknown_opts) (unknownFlagsErr unknown_opts)

   when (notNull (filter wayRTSOnly (ways dflags))
         && isInterpretiveMode mode) $
        hPutStrLn stderr ("Warning: -debug, -threaded and -ticky are ignored by GHCi")

        -- -prof and --interactive are not a good combination
   when ((filter (not . wayRTSOnly) (ways dflags) /= interpWays)
         && isInterpretiveMode mode
         && not (gopt Opt_ExternalInterpreter dflags)) $
      do throwGhcException (UsageError
              "-fexternal-interpreter is required when using --interactive with a non-standard way (-prof, -static, or -dynamic).")
        -- -ohi sanity check
   if (isJust (outputHi dflags) &&
      (isCompManagerMode mode || srcs `lengthExceeds` 1))
        then throwGhcException (UsageError "-ohi can only be used when compiling a single source file")
        else do

        -- -o sanity checking
   if (srcs `lengthExceeds` 1 && isJust (outputFile dflags)
         && not (isLinkMode mode))
        then throwGhcException (UsageError "can't apply -o to multiple source files")
        else do

   let not_linking = not (isLinkMode mode) || isNoLink (ghcLink dflags)

   when (not_linking && not (null objs)) $
        hPutStrLn stderr ("Warning: the following files would be used as linker inputs, but linking is not being done: " ++ unwords objs)

        -- Check that there are some input files
        -- (except in the interactive case)
   if null srcs && (null objs || not_linking) && needsInputsMode mode
        then throwGhcException (UsageError "no input files")
        else do

   case mode of
      StopBefore HCc | hscTarget dflags /= HscC
        -> throwGhcException $ UsageError $
           "the option -C is only available with an unregisterised GHC"
      _ -> return ()

     -- Verify that output files point somewhere sensible.
   verifyOutputFiles dflags

-- Compiler output options

-- Called to verify that the output files point somewhere valid.
--
-- The assumption is that the directory portion of these output
-- options will have to exist by the time 'verifyOutputFiles'
-- is invoked.
--
-- We create the directories for -odir, -hidir, -outputdir etc. ourselves if
-- they don't exist, so don't check for those here (#2278).
verifyOutputFiles :: DynFlags -> IO ()
verifyOutputFiles dflags = do
  let ofile = outputFile dflags
  when (isJust ofile) $ do
     let fn = fromJust ofile
     flg <- doesDirNameExist fn
     when (not flg) (nonExistentDir "-o" fn)
  let ohi = outputHi dflags
  when (isJust ohi) $ do
     let hi = fromJust ohi
     flg <- doesDirNameExist hi
     when (not flg) (nonExistentDir "-ohi" hi)
 where
   nonExistentDir flg dir =
     throwGhcException (CmdLineError ("error: directory portion of " ++
                             show dir ++ " does not exist (used with " ++
                             show flg ++ " option.)"))

-----------------------------------------------------------------------------
-- GHC modes of operation

type Mode = Either PreStartupMode PostStartupMode
type PostStartupMode = Either PreLoadMode PostLoadMode

data PreStartupMode
  = ShowVersion                          -- ghc -V/--version
  | ShowNumVersion                       -- ghc --numeric-version
  | ShowSupportedExtensions              -- ghc --supported-extensions
  | ShowOptions Bool {- isInteractive -} -- ghc --show-options

showVersionMode, showNumVersionMode, showSupportedExtensionsMode, showOptionsMode :: Mode
showVersionMode             = mkPreStartupMode ShowVersion
showNumVersionMode          = mkPreStartupMode ShowNumVersion
showSupportedExtensionsMode = mkPreStartupMode ShowSupportedExtensions
showOptionsMode             = mkPreStartupMode (ShowOptions False)

mkPreStartupMode :: PreStartupMode -> Mode
mkPreStartupMode = Left

isShowVersionMode :: Mode -> Bool
isShowVersionMode (Left ShowVersion) = True
isShowVersionMode _ = False

isShowNumVersionMode :: Mode -> Bool
isShowNumVersionMode (Left ShowNumVersion) = True
isShowNumVersionMode _ = False

data PreLoadMode
  = ShowGhcUsage                           -- ghc -?
  | ShowGhciUsage                          -- ghci -?
  | ShowInfo                               -- ghc --info
  | PrintWithDynFlags (DynFlags -> String) -- ghc --print-foo

showGhcUsageMode, showGhciUsageMode, showInfoMode :: Mode
showGhcUsageMode = mkPreLoadMode ShowGhcUsage
showGhciUsageMode = mkPreLoadMode ShowGhciUsage
showInfoMode = mkPreLoadMode ShowInfo

printSetting :: String -> Mode
printSetting k = mkPreLoadMode (PrintWithDynFlags f)
    where f dflags = fromMaybe (panic ("Setting not found: " ++ show k))
                   $ lookup k (compilerInfo dflags)

mkPreLoadMode :: PreLoadMode -> Mode
mkPreLoadMode = Right . Left

isShowGhcUsageMode :: Mode -> Bool
isShowGhcUsageMode (Right (Left ShowGhcUsage)) = True
isShowGhcUsageMode _ = False

isShowGhciUsageMode :: Mode -> Bool
isShowGhciUsageMode (Right (Left ShowGhciUsage)) = True
isShowGhciUsageMode _ = False

data PostLoadMode
  = ShowInterface FilePath  -- ghc --show-iface
  | DoMkDependHS            -- ghc -M
  | StopBefore Phase        -- ghc -E | -C | -S
                            -- StopBefore StopLn is the default
  | DoMake                  -- ghc --make
  | DoInteractive           -- ghc --interactive
  | DoEval [String]         -- ghc -e foo -e bar => DoEval ["bar", "foo"]
  | DoAbiHash               -- ghc --abi-hash
  | ShowPackages            -- ghc --show-packages
  | DoFrontend ModuleName   -- ghc --frontend Plugin.Module

doMkDependHSMode, doMakeMode, doInteractiveMode,
  doAbiHashMode, showPackagesMode :: Mode
doMkDependHSMode = mkPostLoadMode DoMkDependHS
doMakeMode = mkPostLoadMode DoMake
doInteractiveMode = mkPostLoadMode DoInteractive
doAbiHashMode = mkPostLoadMode DoAbiHash
showPackagesMode = mkPostLoadMode ShowPackages

showInterfaceMode :: FilePath -> Mode
showInterfaceMode fp = mkPostLoadMode (ShowInterface fp)

stopBeforeMode :: Phase -> Mode
stopBeforeMode phase = mkPostLoadMode (StopBefore phase)

doEvalMode :: String -> Mode
doEvalMode str = mkPostLoadMode (DoEval [str])

doFrontendMode :: String -> Mode
doFrontendMode str = mkPostLoadMode (DoFrontend (mkModuleName str))

mkPostLoadMode :: PostLoadMode -> Mode
mkPostLoadMode = Right . Right

isDoInteractiveMode :: Mode -> Bool
isDoInteractiveMode (Right (Right DoInteractive)) = True
isDoInteractiveMode _ = False

isStopLnMode :: Mode -> Bool
isStopLnMode (Right (Right (StopBefore StopLn))) = True
isStopLnMode _ = False

isDoMakeMode :: Mode -> Bool
isDoMakeMode (Right (Right DoMake)) = True
isDoMakeMode _ = False

isDoEvalMode :: Mode -> Bool
isDoEvalMode (Right (Right (DoEval _))) = True
isDoEvalMode _ = False

#ifdef GHCI
isInteractiveMode :: PostLoadMode -> Bool
isInteractiveMode DoInteractive = True
isInteractiveMode _             = False
#endif

-- isInterpretiveMode: byte-code compiler involved
isInterpretiveMode :: PostLoadMode -> Bool
isInterpretiveMode DoInteractive = True
isInterpretiveMode (DoEval _)    = True
isInterpretiveMode _             = False

needsInputsMode :: PostLoadMode -> Bool
needsInputsMode DoMkDependHS    = True
needsInputsMode (StopBefore _)  = True
needsInputsMode DoMake          = True
needsInputsMode _               = False

-- True if we are going to attempt to link in this mode.
-- (we might not actually link, depending on the GhcLink flag)
isLinkMode :: PostLoadMode -> Bool
isLinkMode (StopBefore StopLn) = True
isLinkMode DoMake              = True
isLinkMode DoInteractive       = True
isLinkMode (DoEval _)          = True
isLinkMode _                   = False

isCompManagerMode :: PostLoadMode -> Bool
isCompManagerMode DoMake        = True
isCompManagerMode DoInteractive = True
isCompManagerMode (DoEval _)    = True
isCompManagerMode _             = False

-- -----------------------------------------------------------------------------
-- Parsing the mode flag

parseModeFlags :: [Located String]
               -> IO (Mode,
                      [Located String],
                      [Located String])
parseModeFlags args = do
  let ((leftover, errs1, warns), (mModeFlag, errs2, flags')) =
          runCmdLine (processArgs mode_flags args)
                     (Nothing, [], [])
      mode = case mModeFlag of
             Nothing     -> doMakeMode
             Just (m, _) -> m

  -- See Note [Handling errors when parsing commandline flags]
  unless (null errs1 && null errs2) $ throwGhcException $ errorsToGhcException $
      map (("on the commandline", )) $ map unLoc errs1 ++ errs2

  return (mode, flags' ++ leftover, warns)

type ModeM = CmdLineP (Maybe (Mode, String), [String], [Located String])
  -- mode flags sometimes give rise to new DynFlags (eg. -C, see below)
  -- so we collect the new ones and return them.

mode_flags :: [Flag ModeM]
mode_flags =
  [  ------- help / version ----------------------------------------------
    defFlag "?"                     (PassFlag (setMode showGhcUsageMode))
  , defFlag "-help"                 (PassFlag (setMode showGhcUsageMode))
  , defFlag "V"                     (PassFlag (setMode showVersionMode))
  , defFlag "-version"              (PassFlag (setMode showVersionMode))
  , defFlag "-numeric-version"      (PassFlag (setMode showNumVersionMode))
  , defFlag "-info"                 (PassFlag (setMode showInfoMode))
  , defFlag "-show-options"         (PassFlag (setMode showOptionsMode))
  , defFlag "-supported-languages"  (PassFlag (setMode showSupportedExtensionsMode))
  , defFlag "-supported-extensions" (PassFlag (setMode showSupportedExtensionsMode))
  , defFlag "-show-packages"        (PassFlag (setMode showPackagesMode))
  ] ++
  [ defFlag k'                      (PassFlag (setMode (printSetting k)))
  | k <- ["Project version",
          "Project Git commit id",
          "Booter version",
          "Stage",
          "Build platform",
          "Host platform",
          "Target platform",
          "Have interpreter",
          "Object splitting supported",
          "Have native code generator",
          "Support SMP",
          "Unregisterised",
          "Tables next to code",
          "RTS ways",
          "Leading underscore",
          "Debug on",
          "LibDir",
          "Global Package DB",
          "C compiler flags",
          "C compiler link flags",
          "ld flags"],
    let k' = "-print-" ++ map (replaceSpace . toLower) k
        replaceSpace ' ' = '-'
        replaceSpace c   = c
  ] ++
      ------- interfaces ----------------------------------------------------
  [ defFlag "-show-iface"  (HasArg (\f -> setMode (showInterfaceMode f)
                                               "--show-iface"))

      ------- primary modes ------------------------------------------------
  , defFlag "c"            (PassFlag (\f -> do setMode (stopBeforeMode StopLn) f
                                               addFlag "-no-link" f))
  , defFlag "M"            (PassFlag (setMode doMkDependHSMode))
  , defFlag "E"            (PassFlag (setMode (stopBeforeMode anyHsc)))
  , defFlag "C"            (PassFlag (setMode (stopBeforeMode HCc)))
  , defFlag "S"            (PassFlag (setMode (stopBeforeMode (As False))))
  , defFlag "-make"        (PassFlag (setMode doMakeMode))
  , defFlag "-interactive" (PassFlag (setMode doInteractiveMode))
  , defFlag "-abi-hash"    (PassFlag (setMode doAbiHashMode))
  , defFlag "e"            (SepArg   (\s -> setMode (doEvalMode s) "-e"))
  , defFlag "-frontend"    (SepArg   (\s -> setMode (doFrontendMode s) "-frontend"))
  ]

setMode :: Mode -> String -> EwM ModeM ()
setMode newMode newFlag = liftEwM $ do
    (mModeFlag, errs, flags') <- getCmdLineState
    let (modeFlag', errs') =
            case mModeFlag of
            Nothing -> ((newMode, newFlag), errs)
            Just (oldMode, oldFlag) ->
                case (oldMode, newMode) of
                    -- -c/--make are allowed together, and mean --make -no-link
                    _ |  isStopLnMode oldMode && isDoMakeMode newMode
                      || isStopLnMode newMode && isDoMakeMode oldMode ->
                      ((doMakeMode, "--make"), [])

                    -- If we have both --help and --interactive then we
                    -- want showGhciUsage
                    _ | isShowGhcUsageMode oldMode &&
                        isDoInteractiveMode newMode ->
                            ((showGhciUsageMode, oldFlag), [])
                      | isShowGhcUsageMode newMode &&
                        isDoInteractiveMode oldMode ->
                            ((showGhciUsageMode, newFlag), [])

                    -- If we have both -e and --interactive then -e always wins
                    _ | isDoEvalMode oldMode &&
                        isDoInteractiveMode newMode ->
                            ((oldMode, oldFlag), [])
                      | isDoEvalMode newMode &&
                        isDoInteractiveMode oldMode ->
                            ((newMode, newFlag), [])

                    -- Otherwise, --help/--version/--numeric-version always win
                      | isDominantFlag oldMode -> ((oldMode, oldFlag), [])
                      | isDominantFlag newMode -> ((newMode, newFlag), [])
                    -- We need to accumulate eval flags like "-e foo -e bar"
                    (Right (Right (DoEval esOld)),
                     Right (Right (DoEval [eNew]))) ->
                        ((Right (Right (DoEval (eNew : esOld))), oldFlag),
                         errs)
                    -- Saying e.g. --interactive --interactive is OK
                    _ | oldFlag == newFlag -> ((oldMode, oldFlag), errs)

                    -- --interactive and --show-options are used together
                    (Right (Right DoInteractive), Left (ShowOptions _)) ->
                      ((Left (ShowOptions True),
                        "--interactive --show-options"), errs)
                    (Left (ShowOptions _), (Right (Right DoInteractive))) ->
                      ((Left (ShowOptions True),
                        "--show-options --interactive"), errs)
                    -- Otherwise, complain
                    _ -> let err = flagMismatchErr oldFlag newFlag
                         in ((oldMode, oldFlag), err : errs)
    putCmdLineState (Just modeFlag', errs', flags')
  where isDominantFlag f = isShowGhcUsageMode   f ||
                           isShowGhciUsageMode  f ||
                           isShowVersionMode    f ||
                           isShowNumVersionMode f

flagMismatchErr :: String -> String -> String
flagMismatchErr oldFlag newFlag
    = "cannot use `" ++ oldFlag ++  "' with `" ++ newFlag ++ "'"

addFlag :: String -> String -> EwM ModeM ()
addFlag s flag = liftEwM $ do
  (m, e, flags') <- getCmdLineState
  putCmdLineState (m, e, mkGeneralLocated loc s : flags')
    where loc = "addFlag by " ++ flag ++ " on the commandline"

-- ----------------------------------------------------------------------------
-- Run --make mode

doMake :: [(String,Maybe Phase)] -> Ghc ()
doMake srcs  = do
    let (hs_srcs, non_hs_srcs) = partition isHaskellishTarget srcs

    hsc_env <- GHC.getSession

    -- if we have no haskell sources from which to do a dependency
    -- analysis, then just do one-shot compilation and/or linking.
    -- This means that "ghc Foo.o Bar.o -o baz" links the program as
    -- we expect.
    if (null hs_srcs)
       then liftIO (oneShot hsc_env StopLn srcs)
       else do

    o_files <- mapM (\x -> liftIO $ compileFile hsc_env StopLn x)
                 non_hs_srcs
    dflags <- GHC.getSessionDynFlags
    let dflags' = dflags { ldInputs = map (FileOption "") o_files
                                      ++ ldInputs dflags }
    _ <- GHC.setSessionDynFlags dflags'

    targets <- mapM (uncurry GHC.guessTarget) hs_srcs
    GHC.setTargets targets
    ok_flag <- GHC.load LoadAllTargets

    when (failed ok_flag) (liftIO $ exitWith (ExitFailure 1))

    return ()

-- <prana>
    mgraph <- GHC.getModuleGraph
    mapM_
      (\modSummary -> do
         liftIO (hPutStrLn stderr ("Writing " ++ moduleToFilePath (GHC.ms_mod modSummary)))
         bs <- compile modSummary
         liftIO
           (S.writeFile
                (moduleToFilePath (GHC.ms_mod modSummary))
                (encode (map toBind bs))))
      mgraph

toBind :: CoreSyn.Bind GHC.Var -> Main.Bind
toBind = \case
  CoreSyn.NonRec v e -> Main.NonRec (toVar v) (toExp e)
  CoreSyn.Rec bs -> Main.Rec (map (\(v,e) -> (toVar v,toExp e)) bs)

toExp :: CoreSyn.Expr GHC.Var -> Main.Exp
toExp = \case
 CoreSyn.Var i -> Main.VarE (toId i)
 CoreSyn.Lit i                  -> Main.LitE (toLit i)
 CoreSyn.App f x                -> Main.AppE (toExp f) (toExp x)
 CoreSyn.Lam var body           -> Main.LamE (toVar var) (toExp body)
 CoreSyn.Let bind expr          -> Main.LetE (toBind bind) (toExp expr)
 CoreSyn.Case expr var typ alts -> Main.CaseE (toExp expr) (toVar var) (toTyp typ) (map toAlt alts)
 CoreSyn.Cast expr _coercion    -> Main.CastE (toExp expr)
 CoreSyn.Tick _tickishVar expr  -> toExp expr
 CoreSyn.Type typ               -> Main.TypE (toTyp typ)
 CoreSyn.Coercion _coercion     -> Main.CoercionE

toAlt :: (CoreSyn.AltCon, [GHC.Var], CoreSyn.Expr GHC.Var) -> Alt
toAlt (con,vars,e) = Alt (toAltCon con) (map toVar vars) (toExp e)

toAltCon :: CoreSyn.AltCon -> Main.AltCon
toAltCon =
  \case
    CoreSyn.DataAlt dataCon -> DataAlt (toDataCon dataCon)
    CoreSyn.LitAlt literal  -> LitAlt (toLit literal)
    CoreSyn.DEFAULT         -> DEFAULT

toLit :: GHC.Literal -> Main.Lit
toLit =
  \case
    GHC.MachChar i          -> Char i
    GHC.MachStr i           -> Str i
    GHC.MachNullAddr        -> NullAddr
    GHC.MachInt i           -> Int i
    GHC.MachInt64 i         -> Int64 i
    GHC.MachWord i          -> Word i
    GHC.MachWord64 i        -> Word64 i
    GHC.MachFloat (i)  -> Float i
    GHC.MachDouble (i) -> Double i
    GHC.MachLabel _ _ _     -> Label
    GHC.LitInteger i _typ   -> Integer i

toTyp :: GHC.Type -> Main.Typ
toTyp v = Main.Typ (S8.pack (GHC.showSDocUnsafe (GHC.ppr v)))

toDataCon :: GHC.DataCon -> Main.DataCon
toDataCon = Main.DataCon . S.pack . GHC.dataConIdentity

toId :: GHC.Id -> Main.Id
toId v = Main.Id (S8.pack (GHC.nameStableString (GHC.getName v)))

toVar :: GHC.Var -> Main.Var
toVar v = Main.Var (S8.pack (GHC.nameStableString (GHC.getName v)))
-- </prana>

-- <prana>
compile ::
     GHC.GhcMonad m
  => GHC.ModSummary
  -> m [CoreSyn.Bind GHC.Var]
compile modSummary = do
  parsedModule <- GHC.parseModule modSummary
  typecheckedModule <- GHC.typecheckModule parsedModule
  desugared <- GHC.desugarModule typecheckedModule
  let binds = GHC.mg_binds (GHC.dm_core_module desugared)
  pure binds

moduleToFilePath :: GHC.Module -> FilePath
moduleToFilePath module' = packageNameVersion ++ "_" ++ moduleNameString ++ ".prana"
  where
    unitId = GHC.moduleUnitId module'
    moduleName_ = GHC.moduleName module'
    packageNameVersion = GHC.unitIdString unitId
    moduleNameString = GHC.moduleNameString moduleName_
-- </prana>

-- ---------------------------------------------------------------------------
-- --show-iface mode

doShowIface :: DynFlags -> FilePath -> IO ()
doShowIface dflags file = do
  hsc_env <- newHscEnv dflags
  showIface hsc_env file

-- ---------------------------------------------------------------------------
-- Various banners and verbosity output.

showBanner :: PostLoadMode -> DynFlags -> IO ()
showBanner _postLoadMode dflags = do
   let verb = verbosity dflags

#ifdef GHCI
   -- Show the GHCi banner
   when (isInteractiveMode _postLoadMode && verb >= 1) $ putStrLn ghciWelcomeMsg
#endif

   -- Display details of the configuration in verbose mode
   when (verb >= 2) $
    do hPutStr stderr "Glasgow Haskell Compiler, Version "
       hPutStr stderr cProjectVersion
       hPutStr stderr ", stage "
       hPutStr stderr cStage
       hPutStr stderr " booted by GHC version "
       hPutStrLn stderr cBooterVersion

-- We print out a Read-friendly string, but a prettier one than the
-- Show instance gives us
showInfo :: DynFlags -> IO ()
showInfo dflags = do
        let sq x = " [" ++ x ++ "\n ]"
        putStrLn $ sq $ intercalate "\n ," $ map show $ compilerInfo dflags

showSupportedExtensions :: IO ()
showSupportedExtensions = mapM_ putStrLn supportedLanguagesAndExtensions

showVersion :: IO ()
showVersion = putStrLn (cProjectName ++ ", version " ++ cProjectVersion)

showOptions :: Bool -> IO ()
showOptions isInteractive = putStr (unlines availableOptions)
    where
      availableOptions = concat [
        flagsForCompletion isInteractive,
        map ('-':) (concat [
            getFlagNames mode_flags
          , (filterUnwantedStatic . getFlagNames $ flagsStatic)
          , flagsStaticNames
          ])
        ]
      getFlagNames opts         = map flagName opts
      -- this is a hack to get rid of two unwanted entries that get listed
      -- as static flags. Hopefully this hack will disappear one day together
      -- with static flags
      filterUnwantedStatic      = filter (`notElem`["f", "fno-"])

showGhcUsage :: DynFlags -> IO ()
showGhcUsage = showUsage False

showGhciUsage :: DynFlags -> IO ()
showGhciUsage = showUsage True

showUsage :: Bool -> DynFlags -> IO ()
showUsage ghci dflags = do
  let usage_path = if ghci then ghciUsagePath dflags
                           else ghcUsagePath dflags
  usage <- readFile usage_path
  dump usage
  where
     dump ""          = return ()
     dump ('$':'$':s) = putStr progName >> dump s
     dump (c:s)       = putChar c >> dump s

dumpFinalStats :: DynFlags -> IO ()
dumpFinalStats dflags =
  when (gopt Opt_D_faststring_stats dflags) $ dumpFastStringStats dflags

dumpFastStringStats :: DynFlags -> IO ()
dumpFastStringStats dflags = do
  buckets <- getFastStringTable
  let (entries, longest, has_z) = countFS 0 0 0 buckets
      msg = text "FastString stats:" $$
            nest 4 (vcat [text "size:           " <+> int (length buckets),
                          text "entries:        " <+> int entries,
                          text "longest chain:  " <+> int longest,
                          text "has z-encoding: " <+> (has_z `pcntOf` entries)
                         ])
        -- we usually get more "has z-encoding" than "z-encoded", because
        -- when we z-encode a string it might hash to the exact same string,
        -- which will is not counted as "z-encoded".  Only strings whose
        -- Z-encoding is different from the original string are counted in
        -- the "z-encoded" total.
  putMsg dflags msg
  where
   x `pcntOf` y = int ((x * 100) `quot` y) Outputable.<> char '%'

countFS :: Int -> Int -> Int -> [[FastString]] -> (Int, Int, Int)
countFS entries longest has_z [] = (entries, longest, has_z)
countFS entries longest has_z (b:bs) =
  let
        len = length b
        longest' = max len longest
        entries' = entries + len
        has_zs = length (filter hasZEncoding b)
  in
        countFS entries' longest' (has_z + has_zs) bs

showPackages, dumpPackages, dumpPackagesSimple :: DynFlags -> IO ()
showPackages       dflags = putStrLn (showSDoc dflags (pprPackages dflags))
dumpPackages       dflags = putMsg dflags (pprPackages dflags)
dumpPackagesSimple dflags = putMsg dflags (pprPackagesSimple dflags)

-- -----------------------------------------------------------------------------
-- Frontend plugin support

doFrontend :: ModuleName -> [(String, Maybe Phase)] -> Ghc ()
#ifndef GHCI
doFrontend modname _ = pluginError [modname]
#else
doFrontend modname srcs = do
    hsc_env <- getSession
    frontend_plugin <- liftIO $ loadFrontendPlugin hsc_env modname
    frontend frontend_plugin (frontendPluginOpts (hsc_dflags hsc_env)) srcs
#endif

-- -----------------------------------------------------------------------------
-- ABI hash support

{-
        ghc --abi-hash Data.Foo System.Bar

Generates a combined hash of the ABI for modules Data.Foo and
System.Bar.  The modules must already be compiled, and appropriate -i
options may be necessary in order to find the .hi files.

This is used by Cabal for generating the ComponentId for a
package.  The ComponentId must change when the visible ABI of
the package chagnes, so during registration Cabal calls ghc --abi-hash
to get a hash of the package's ABI.
-}

-- | Print ABI hash of input modules.
--
-- The resulting hash is the MD5 of the GHC version used (Trac #5328,
-- see 'hiVersion') and of the existing ABI hash from each module (see
-- 'mi_mod_hash').
abiHash :: [String] -- ^ List of module names
        -> Ghc ()
abiHash strs = do
  hsc_env <- getSession
  let dflags = hsc_dflags hsc_env

  liftIO $ do

  let find_it str = do
         let modname = mkModuleName str
         r <- findImportedModule hsc_env modname Nothing
         case r of
           Found _ m -> return m
           _error    -> throwGhcException $ CmdLineError $ showSDoc dflags $
                          cannotFindInterface dflags modname r

  mods <- mapM find_it strs

  let get_iface modl = loadUserInterface False (text "abiHash") modl
  ifaces <- initIfaceCheck hsc_env $ mapM get_iface mods

  bh <- openBinMem (3*1024) -- just less than a block
  put_ bh hiVersion
    -- package hashes change when the compiler version changes (for now)
    -- see #5328
  mapM_ (put_ bh . mi_mod_hash) ifaces
  f <- fingerprintBinMem bh

  putStrLn (showPpr dflags f)

-- -----------------------------------------------------------------------------
-- Util

unknownFlagsErr :: [String] -> a
unknownFlagsErr fs = throwGhcException $ UsageError $ concatMap oneError fs
  where
    oneError f =
        "unrecognised flag: " ++ f ++ "\n" ++
        (case fuzzyMatch f (nub allNonDeprecatedFlags) of
            [] -> ""
            suggs -> "did you mean one of:\n" ++ unlines (map ("  " ++) suggs))

{- Note [-Bsymbolic and hooks]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-Bsymbolic is a flag that prevents the binding of references to global
symbols to symbols outside the shared library being compiled (see `man
ld`). When dynamically linking, we don't use -Bsymbolic on the RTS
package: that is because we want hooks to be overridden by the user,
we don't want to constrain them to the RTS package.

Unfortunately this seems to have broken somehow on OS X: as a result,
defaultHooks (in hschooks.c) is not called, which does not initialize
the GC stats. As a result, this breaks things like `:set +s` in GHCi
(#8754). As a hacky workaround, we instead call 'defaultHooks'
directly to initalize the flags in the RTS.

A byproduct of this, I believe, is that hooks are likely broken on OS
X when dynamically linking. But this probably doesn't affect most
people since we're linking GHC dynamically, but most things themselves
link statically.
-}

foreign import ccall safe "initGCStatistics"
  initGCStatistics :: IO ()

--------------------------------------------------------------------------------
-- Persist package


#include "MachDeps.h"

data a :!: b = !a :!: !b
infixl 2 :!:

newtype BigEndian a = BigEndian { unBE :: a }
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

newtype LittleEndian a = LittleEndian { unLE :: a }
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

class Persist t where
  -- | Encode a value in the Put monad.
  put :: t -> Put ()
  -- | Decode a value in the Get monad
  get :: Get t

  default put :: (Generic t, GPersistPut (Rep t)) => t -> Put ()
  put = gput . from

  default get :: (Generic t, GPersistGet (Rep t)) => Get t
  get = to <$> gget

-- | Encode a value using binary serialization to a strict ByteString.
encode :: Persist a => a -> ByteString
encode = runPut . put

putLE :: Persist (LittleEndian a) => a -> Put ()
putLE = put . LittleEndian
{-# INLINE putLE #-}

putBE :: Persist (BigEndian a) => a -> Put ()
putBE = put . BigEndian
{-# INLINE putBE #-}

getLE :: Persist (LittleEndian a) => Get a
getLE = unLE <$> get
{-# INLINE getLE #-}

getBE :: Persist (BigEndian a) => Get a
getBE = unBE <$> get
{-# INLINE getBE #-}

unsafePutByte :: Integral a => a -> Put ()
unsafePutByte x = Put $ \_ p -> do
  poke p $ fromIntegral x
  pure $! p `plusPtr` 1 :!: ()
{-# INLINE unsafePutByte #-}

unsafeGetByte :: Num a => Get a
unsafeGetByte = Get $ \_ p -> do
  x <- peek p
  pure $! p `plusPtr` 1 :!: fromIntegral x
{-# INLINE unsafeGetByte #-}

reinterpretCast :: (Storable a, Storable b) => Ptr p -> a -> IO b
reinterpretCast p x = do
  poke (castPtr p) x
  peek (castPtr p)
{-# INLINE reinterpretCast #-}

reinterpretCastPut :: (Storable a, Storable b) => a -> Put b
reinterpretCastPut x = Put $ \e p -> (p :!:) <$!> reinterpretCast (peTmp e) x
{-# INLINE reinterpretCastPut #-}

reinterpretCastGet :: (Storable a, Storable b) => a -> Get b
reinterpretCastGet x = Get $ \e p -> (p :!:) <$!> reinterpretCast (geTmp e) x
{-# INLINE reinterpretCastGet #-}

-- The () type need never be written to disk: values of singleton type
-- can be reconstructed from the type alone
instance Persist () where
  put () = pure ()
  {-# INLINE put #-}
  get = pure ()
  {-# INLINE get #-}

instance Persist Word8 where
  put x = do
    grow 1
    unsafePutByte x
  {-# INLINE put #-}

  get = do
    ensure 1
    unsafeGetByte
  {-# INLINE get #-}

instance Persist (LittleEndian Word16) where
  put x = do
    grow 2
    let y = unLE x
    unsafePutByte $ y .&. 0xFF
    unsafePutByte $ y `unsafeShiftR` 8
  {-# INLINE put #-}

  get = do
    ensure 2
    x0 <- unsafeGetByte
    x1 <- unsafeGetByte
    pure $ LittleEndian $
      x1 `unsafeShiftL` 8
      .|. x0
  {-# INLINE get #-}

instance Persist (BigEndian Word16) where
  put x = do
    grow 2
    let y = unBE x
    unsafePutByte $ y `unsafeShiftR` 8
    unsafePutByte $ y .&. 0xFF
  {-# INLINE put #-}

  get = do
    ensure 2
    x1 <- unsafeGetByte
    x0 <- unsafeGetByte
    pure $ BigEndian $
      x1 `unsafeShiftL` 8
      .|. x0
  {-# INLINE get #-}

instance Persist Word16 where
  put = putLE
  {-# INLINE put #-}
  get = getLE
  {-# INLINE get #-}

instance Persist (LittleEndian Word32) where
  put x = do
    grow 4
    let y = unLE x
    unsafePutByte $ y .&. 0xFF
    unsafePutByte $ y `unsafeShiftR` 8 .&. 0xFF
    unsafePutByte $ y `unsafeShiftR` 16 .&. 0xFF
    unsafePutByte $ y `unsafeShiftR` 24
  {-# INLINE put #-}

  get = do
    ensure 4
    x0 <- unsafeGetByte
    x1 <- unsafeGetByte
    x2 <- unsafeGetByte
    x3 <- unsafeGetByte
    pure $ LittleEndian $
      x3 `unsafeShiftL` 24
      .|. x2 `unsafeShiftL` 16
      .|. x1 `unsafeShiftL` 8
      .|. x0
  {-# INLINE get #-}

instance Persist (BigEndian Word32) where
  put x = do
    grow 4
    let y = unBE x
    unsafePutByte $ y `unsafeShiftR` 24
    unsafePutByte $ y `unsafeShiftR` 16 .&. 0xFF
    unsafePutByte $ y `unsafeShiftR` 8 .&. 0xFF
    unsafePutByte $ y .&. 0xFF
  {-# INLINE put #-}

  get = do
    ensure 4
    x3 <- unsafeGetByte
    x2 <- unsafeGetByte
    x1 <- unsafeGetByte
    x0 <- unsafeGetByte
    pure $ BigEndian $
      x3 `unsafeShiftL` 24
      .|. x2 `unsafeShiftL` 16
      .|. x1 `unsafeShiftL` 8
      .|. x0
  {-# INLINE get #-}

instance Persist Word32 where
  put = putLE
  {-# INLINE put #-}
  get = getLE
  {-# INLINE get #-}

instance Persist (LittleEndian Word64) where
  put x = do
    grow 8
    let y = unLE x
    unsafePutByte $ y .&. 0xFF
    unsafePutByte $ y `unsafeShiftR` 8 .&. 0xFF
    unsafePutByte $ y `unsafeShiftR` 16 .&. 0xFF
    unsafePutByte $ y `unsafeShiftR` 24 .&. 0xFF
    unsafePutByte $ y `unsafeShiftR` 32 .&. 0xFF
    unsafePutByte $ y `unsafeShiftR` 40 .&. 0xFF
    unsafePutByte $ y `unsafeShiftR` 48 .&. 0xFF
    unsafePutByte $ y `unsafeShiftR` 56
  {-# INLINE put #-}

  get = do
    ensure 8
    x0 <- unsafeGetByte
    x1 <- unsafeGetByte
    x2 <- unsafeGetByte
    x3 <- unsafeGetByte
    x4 <- unsafeGetByte
    x5 <- unsafeGetByte
    x6 <- unsafeGetByte
    x7 <- unsafeGetByte
    pure $ LittleEndian $
      x7 `unsafeShiftL` 56
      .|. x6 `unsafeShiftL` 48
      .|. x5 `unsafeShiftL` 40
      .|. x4 `unsafeShiftL` 32
      .|. x3 `unsafeShiftL` 24
      .|. x2 `unsafeShiftL` 16
      .|. x1 `unsafeShiftL` 8
      .|. x0
  {-# INLINE get #-}

instance Persist (BigEndian Word64) where
  put x = do
    grow 8
    let y = unBE x
    unsafePutByte $ y `unsafeShiftR` 56
    unsafePutByte $ y `unsafeShiftR` 48 .&. 0xFF
    unsafePutByte $ y `unsafeShiftR` 40 .&. 0xFF
    unsafePutByte $ y `unsafeShiftR` 32 .&. 0xFF
    unsafePutByte $ y `unsafeShiftR` 24 .&. 0xFF
    unsafePutByte $ y `unsafeShiftR` 16 .&. 0xFF
    unsafePutByte $ y `unsafeShiftR` 8 .&. 0xFF
    unsafePutByte $ y .&. 0xFF
  {-# INLINE put #-}

  get = do
    ensure 8
    x7 <- unsafeGetByte
    x6 <- unsafeGetByte
    x5 <- unsafeGetByte
    x4 <- unsafeGetByte
    x3 <- unsafeGetByte
    x2 <- unsafeGetByte
    x1 <- unsafeGetByte
    x0 <- unsafeGetByte
    pure $ BigEndian $
      x7 `unsafeShiftL` 56
      .|. x6 `unsafeShiftL` 48
      .|. x5 `unsafeShiftL` 40
      .|. x4 `unsafeShiftL` 32
      .|. x3 `unsafeShiftL` 24
      .|. x2 `unsafeShiftL` 16
      .|. x1 `unsafeShiftL` 8
      .|. x0
  {-# INLINE get #-}

instance Persist Word64 where
  put = putLE
  {-# INLINE put #-}
  get = getLE
  {-# INLINE get #-}

instance Persist Int8 where
  put = put @Word8 . fromIntegral
  {-# INLINE put #-}
  get = fromIntegral <$> get @Word8
  {-# INLINE get #-}

instance Persist (LittleEndian Int16) where
  put = put . fmap (fromIntegral @_ @Word16)
  {-# INLINE put #-}
  get = fmap (fromIntegral @Word16) <$> get
  {-# INLINE get #-}

instance Persist (BigEndian Int16) where
  put = put . fmap (fromIntegral @_ @Word16)
  {-# INLINE put #-}
  get = fmap (fromIntegral @Word16) <$> get
  {-# INLINE get #-}

instance Persist Int16 where
  put = putLE
  {-# INLINE put #-}
  get = getLE
  {-# INLINE get #-}

instance Persist (LittleEndian Int32) where
  put = put . fmap (fromIntegral @_ @Word32)
  {-# INLINE put #-}
  get = fmap (fromIntegral @Word32) <$> get
  {-# INLINE get #-}

instance Persist (BigEndian Int32) where
  put = put . fmap (fromIntegral @_ @Word32)
  {-# INLINE put #-}
  get = fmap (fromIntegral @Word32) <$> get
  {-# INLINE get #-}

instance Persist Int32 where
  put = putLE
  {-# INLINE put #-}
  get = getLE
  {-# INLINE get #-}

instance Persist (LittleEndian Int64) where
  put = put . fmap (fromIntegral @_ @Word64)
  {-# INLINE put #-}
  get = fmap (fromIntegral @Word64) <$> get
  {-# INLINE get #-}

instance Persist (BigEndian Int64) where
  put = put . fmap (fromIntegral @_ @Word64)
  {-# INLINE put #-}
  get = fmap (fromIntegral @Word64) <$> get
  {-# INLINE get #-}

instance Persist Int64 where
  put = putLE
  {-# INLINE put #-}
  get = getLE
  {-# INLINE get #-}

instance Persist (LittleEndian Double) where
  put x = reinterpretCastPut (unLE x) >>= putLE @Word64
  {-# INLINE put #-}
  get = getLE @Word64 >>= fmap LittleEndian . reinterpretCastGet
  {-# INLINE get #-}

instance Persist (BigEndian Double) where
  put x = reinterpretCastPut (unBE x) >>= putBE @Word64
  {-# INLINE put #-}
  get = getBE @Word64 >>= fmap BigEndian . reinterpretCastGet
  {-# INLINE get #-}

instance Persist Double where
  put = putLE
  {-# INLINE put #-}
  get = getLE
  {-# INLINE get #-}

instance Persist (LittleEndian Float) where
  put x = reinterpretCastPut (unLE x) >>= putLE @Word32
  {-# INLINE put #-}
  get = getLE @Word32 >>= fmap LittleEndian . reinterpretCastGet
  {-# INLINE get #-}

instance Persist (BigEndian Float) where
  put x = reinterpretCastPut (unBE x) >>= putBE @Word32
  {-# INLINE put #-}
  get = getBE @Word32 >>= fmap BigEndian . reinterpretCastGet
  {-# INLINE get #-}

instance Persist Float where
  put = putLE
  {-# INLINE put #-}
  get = getLE
  {-# INLINE get #-}

instance Persist (LittleEndian Word) where
  put = put . fmap (fromIntegral @_ @Word64)
  {-# INLINE put #-}
  get = fmap (fromIntegral @Word64) <$> get
  {-# INLINE get #-}

instance Persist (BigEndian Word) where
  put = put . fmap (fromIntegral @_ @Word64)
  {-# INLINE put #-}
  get = fmap (fromIntegral @Word64) <$> get
  {-# INLINE get #-}

instance Persist Word where
  put = putLE
  {-# INLINE put #-}
  get = getLE
  {-# INLINE get #-}

instance Persist (LittleEndian Int) where
  put = put . fmap (fromIntegral @_ @Int64)
  {-# INLINE put #-}
  get = fmap (fromIntegral @Int64) <$> get
  {-# INLINE get #-}

instance Persist (BigEndian Int) where
  put = put . fmap (fromIntegral @_ @Int64)
  {-# INLINE put #-}
  get = fmap (fromIntegral @Int64) <$> get
  {-# INLINE get #-}

instance Persist Int where
  put = putLE
  {-# INLINE put #-}
  get = getLE
  {-# INLINE get #-}

instance Persist Integer where
  put n = do
    put $ n < 0
    put $ unroll $ abs n

  get = do
    neg <- get
    val <- roll <$> get
    pure $! if neg then negate val else val

unroll :: (Integral a, Bits a) => a -> [Word8]
unroll = unfoldr step
  where step 0 = Nothing
        step i = Just (fromIntegral i, i `unsafeShiftR` 8)

roll :: (Integral a, Bits a) => [Word8] -> a
roll = foldr unstep 0
  where unstep b a = a `unsafeShiftL` 8 .|. fromIntegral b

instance Persist a => Persist (Ratio a) where
  put (n :% d) = put n *> put d
  {-# INLINE put #-}

  get = (:%) <$> get <*> get
  {-# INLINE get #-}

instance Persist Natural where
  put = put . unroll
  get = roll <$> get

-- Char is serialized as UTF-8
instance Persist Char where
  put a | c <= 0x7f     = put (fromIntegral c :: Word8)
        | c <= 0x7ff    = do put (0xc0 .|. y)
                             put (0x80 .|. z)
        | c <= 0xffff   = do put (0xe0 .|. x)
                             put (0x80 .|. y)
                             put (0x80 .|. z)
        | c <= 0x10ffff = do put (0xf0 .|. w)
                             put (0x80 .|. x)
                             put (0x80 .|. y)
                             put (0x80 .|. z)
        | otherwise = error "Not a valid Unicode code point"
    where
      c = ord a
      z, y, x, w :: Word8
      z = fromIntegral (c                 .&. 0x3f)
      y = fromIntegral (unsafeShiftR c 6  .&. 0x3f)
      x = fromIntegral (unsafeShiftR c 12 .&. 0x3f)
      w = fromIntegral (unsafeShiftR c 18 .&. 0x7)
  {-# INLINE put #-}

  get = do
    let byte = fromIntegral <$> get @Word8
        shiftL6 = flip unsafeShiftL 6
    w <- byte
    r <- if | w < 0x80  -> pure w
            | w < 0xe0  -> do
                x <- xor 0x80 <$> byte
                pure $ x .|. shiftL6 (xor 0xc0 w)
            | w < 0xf0  -> do
                x <- xor 0x80 <$> byte
                y <- xor 0x80 <$> byte
                pure $ y .|. shiftL6 (x .|. shiftL6
                                       (xor 0xe0 w))
            | otherwise -> do
                x <- xor 0x80 <$> byte
                y <- xor 0x80 <$> byte
                z <- xor 0x80 <$> byte
                pure $ z .|. shiftL6 (y .|. shiftL6
                                       (x .|. shiftL6 (xor 0xf0 w)))
    if r < 0x10FFFF then
      pure $ unsafeChr r
    else
      fail "Invalid character"
  {-# INLINE get #-}

instance Persist Bool
instance Persist Ordering
instance (Persist a) => Persist (Maybe a)
instance (Persist a, Persist b) => Persist (Either a b)
instance (Persist a, Persist b) => Persist (a,b)
instance (Persist a, Persist b, Persist c) => Persist (a,b,c)
instance (Persist a, Persist b, Persist c, Persist d)
        => Persist (a,b,c,d)
instance (Persist a, Persist b, Persist c, Persist d, Persist e)
        => Persist (a,b,c,d,e)
instance (Persist a, Persist b, Persist c, Persist d, Persist e
         , Persist f)
        => Persist (a,b,c,d,e,f)
instance (Persist a, Persist b, Persist c, Persist d, Persist e
         , Persist f, Persist g)
        => Persist (a,b,c,d,e,f,g)
instance Persist a => Persist (M.Dual a)
instance Persist M.All
instance Persist M.Any
instance Persist a => Persist (M.Sum a)
instance Persist a => Persist (M.Product a)
instance Persist a => Persist (M.First a)
instance Persist a => Persist (M.Last a)

-- | Persist a list in the following format:
--   Word64 (little endian format)
--   element 1
--   ...
--   element n
instance Persist a => Persist [a] where
    put l = do
      put $ length l
      mapM_ put l
    {-# INLINE put #-}

    get = go [] =<< get @Word64
      where go as 0 = pure $! reverse as
            go as i = do x <- get
                         x `seq` go (x:as) (i - 1)
    {-# INLINE get #-}

instance Persist ByteString where
  put s = do
    put $ B.length s
    putByteString s
  get = get >>= getByteString

instance Persist L.ByteString where
  put = put . L.toStrict
  get = L.fromStrict <$!> get

type family SumArity (a :: * -> *) :: Nat where
  SumArity (C1 c a) = 1
  SumArity (x :+: y) = SumArity x + SumArity y

class GPersistPut f where
  gput :: f a -> Put ()

class GPersistGet f where
  gget :: Get (f a)

instance GPersistPut f => GPersistPut (M1 i c f) where
  gput = gput . unM1
  {-# INLINE gput #-}

instance GPersistGet f => GPersistGet (M1 i c f) where
  gget = fmap M1 gget
  {-# INLINE gget #-}

instance Persist a => GPersistPut (K1 i a) where
  gput = put . unK1
  {-# INLINE gput #-}

instance Persist a => GPersistGet (K1 i a) where
  gget = fmap K1 get
  {-# INLINE gget #-}

instance GPersistPut U1 where
  gput _ = pure ()
  {-# INLINE gput #-}

instance GPersistGet U1 where
  gget = pure U1
  {-# INLINE gget #-}

instance GPersistPut V1 where
  gput x = case x of {}
  {-# INLINE gput #-}

instance GPersistGet V1 where
  gget = undefined
  {-# INLINE gget #-}

instance (GPersistPut a, GPersistPut b) => GPersistPut (a :*: b) where
  gput (a :*: b) = gput a *> gput b
  {-# INLINE gput #-}

instance (GPersistGet a, GPersistGet b) => GPersistGet (a :*: b) where
  gget = (:*:) <$> gget <*> gget
  {-# INLINE gget #-}

instance (SumArity (a :+: b) <= 255, GPersistPutSum 0 (a :+: b)) => GPersistPut (a :+: b) where
  gput x = gputSum x (Proxy :: Proxy 0)
  {-# INLINE gput #-}

instance (SumArity (a :+: b) <= 255, GPersistGetSum 0 (a :+: b)) => GPersistGet (a :+: b) where
  gget = do
    tag <- get
    ggetSum tag (Proxy :: Proxy 0)
  {-# INLINE gget #-}

class KnownNat n => GPersistPutSum (n :: Nat) (f :: * -> *) where
  gputSum :: f p -> Proxy n -> Put ()

class KnownNat n => GPersistGetSum (n :: Nat) (f :: * -> *) where
  ggetSum :: Word8 -> Proxy n -> Get (f p)

instance (GPersistPutSum n a, GPersistPutSum (n + SumArity a) b, KnownNat n)
         => GPersistPutSum n (a :+: b) where
  gputSum (L1 l) _ = gputSum l (Proxy :: Proxy n)
  gputSum (R1 r) _ = gputSum r (Proxy :: Proxy (n + SumArity a))
  {-# INLINE gputSum #-}

instance (GPersistGetSum n a, GPersistGetSum (n + SumArity a) b, KnownNat n)
         => GPersistGetSum n (a :+: b) where
  ggetSum tag proxyL
    | tag < sizeL = L1 <$> ggetSum tag proxyL
    | otherwise = R1 <$> ggetSum tag (Proxy :: Proxy (n + SumArity a))
    where
      sizeL = fromInteger (natVal (Proxy :: Proxy (n + SumArity a)))
  {-# INLINE ggetSum #-}

instance (GPersistPut a, KnownNat n) => GPersistPutSum n (C1 c a) where
  gputSum x _ = do
    put (fromInteger (natVal (Proxy :: Proxy n)) :: Word8)
    gput x
  {-# INLINE gputSum #-}

instance (GPersistGet a, KnownNat n) => GPersistGetSum n (C1 c a) where
  ggetSum tag _
    | tag == cur = gget
    | tag > cur = fail "Sum tag invalid"
    | otherwise = fail "Implementation error"
    where
      cur = fromInteger (natVal (Proxy :: Proxy n))
  {-# INLINE ggetSum #-}
data GetEnv = GetEnv
  { geBuf   :: !(ForeignPtr Word8)
  , geBegin :: !(Ptr Word8)
  , geEnd   :: !(Ptr Word8)
  , geTmp   :: !(Ptr Word8)
  }

newtype Get a = Get
  { unGet :: GetEnv -> Ptr Word8 -> IO (Ptr Word8 :!: a)
  }

instance Functor Get where
  fmap f m = Get $ \e p -> do
    p' :!: x <- unGet m e p
    pure $! p' :!: f x
  {-# INLINE fmap #-}

instance Applicative Get where
  pure a = Get $ \_ p -> pure $! p :!: a
  {-# INLINE pure #-}

  f <*> a = Get $ \e p -> do
    p' :!: f' <- unGet f e p
    p'' :!: a' <- unGet a e p'
    pure $! p'' :!: f' a'
  {-# INLINE (<*>) #-}

  m1 *> m2 = do
    void m1
    m2
  {-# INLINE (*>) #-}

instance Monad Get where
  m >>= f = Get $ \e p -> do
    p' :!: x <- unGet m e p
    unGet (f x) e p'
  {-# INLINE (>>=) #-}

  fail = Fail.fail
  {-# INLINE fail #-}

instance Fail.MonadFail Get where
  fail msg = Get $ \_ _ -> fail $ "Failed reading: " <> msg
  {-# INLINE fail #-}

-- | Run the Get monad applies a 'get'-based parser on the input ByteString
runGet :: Get a -> ByteString -> Either String a
runGet m s = unsafePerformIO $ catch run handler
  where run = withForeignPtr buf $ \p -> allocaBytes 8 $ \t -> do
          let env = GetEnv { geBuf = buf, geBegin = p, geEnd = p `plusPtr` (pos + len), geTmp = t }
          _ :!: r <- unGet m env (p `plusPtr` pos)
          pure $ Right r
        handler (e :: IOException) = pure $ Left $ displayException e
        (B.PS buf pos len) = s
{-# NOINLINE runGet #-}

-- | Ensure that @n@ bytes are available. Fails if fewer than @n@ bytes are available.
ensure :: Int -> Get ()
ensure n
  | n < 0 = fail "ensure: negative length"
  | otherwise = do
      m <- remaining
      when (m < n) $ fail "Not enough bytes available"
{-# INLINE ensure #-}

-- | Skip ahead @n@ bytes. Fails if fewer than @n@ bytes are available.
skip :: Int -> Get ()
skip n = do
  ensure n
  Get $ \_ p -> pure $! p `plusPtr` n :!: ()
{-# INLINE skip #-}

-- | Get the number of remaining unparsed bytes.  Useful for checking whether
-- all input has been consumed.
remaining :: Get Int
remaining = Get $ \e p -> pure $! p :!: geEnd e `minusPtr` p
{-# INLINE remaining #-}

-- -- | Succeed if end of input reached.
eof :: Get ()
eof = do
  n <- remaining
  when (n /= 0) $ fail "Expected end of file"
{-# INLINE eof #-}

-- | Pull @n@ bytes from the input, as a strict ByteString.
getBytes :: Int -> Get ByteString
getBytes n = do
  ensure n
  Get $ \e p -> pure $! p `plusPtr` n :!: B.PS (geBuf e) (p `minusPtr` geBegin e) n
{-# INLINE getBytes #-}

-- | An efficient 'get' method for strict ByteStrings. Fails if fewer
-- than @n@ bytes are left in the input. This function creates a fresh
-- copy of the underlying bytes.
getByteString :: Int -> Get ByteString
getByteString n = B.copy <$!> getBytes n
{-# INLINE getByteString #-}

data Chunk = Chunk
  { chkBegin :: !(Ptr Word8)
  , chkEnd   :: !(Ptr Word8)
  }

data PutEnv = PutEnv
  { peChks :: !(IORef (NonEmpty Chunk))
  , peEnd  :: !(IORef (Ptr Word8))
  , peTmp  :: !(Ptr Word8)
  }

newtype Put a = Put
  { unPut :: PutEnv -> Ptr Word8 -> IO (Ptr Word8 :!: a) }

instance Functor Put where
  fmap f m = Put $ \e p -> do
    p' :!: x <- unPut m e p
    pure $! p' :!: f x
  {-# INLINE fmap #-}

instance Applicative Put where
  pure a = Put $ \_ p -> pure $! p :!: a
  {-# INLINE pure #-}

  f <*> a = Put $ \e p -> do
    p' :!: f' <- unPut f e p
    p'' :!: a' <- unPut a e p'
    pure $! p'' :!: f' a'
  {-# INLINE (<*>) #-}

  m1 *> m2 = do
    void m1
    m2
  {-# INLINE (*>) #-}

instance Monad Put where
  m >>= f = Put $ \e p -> do
    p' :!: x <- unPut m e p
    unPut (f x) e p'
  {-# INLINE (>>=) #-}

minChunkSize :: Int
minChunkSize = 0x10000
{-# INLINE minChunkSize #-}

newChunk :: Int -> IO Chunk
newChunk size = do
  let n = max size minChunkSize
  p <- mallocBytes n
  pure $! Chunk p $ p `plusPtr` n
{-# INLINE newChunk #-}

doGrow :: PutEnv -> Ptr Word8 -> Int -> IO (Ptr Word8 :!: ())
doGrow e p n = do
  k <- newChunk n
  modifyIORef' (peChks e) $ \case
    (c:|cs) -> k :| c { chkEnd = p } : cs
  writeIORef (peEnd e) (chkEnd k)
  pure $! chkBegin k :!: ()
{-# NOINLINE doGrow #-}

-- | Ensure that @n@ bytes can be written.
grow :: Int -> Put ()
grow n
  | n < 0 = fail "grow: negative length"
  | otherwise = Put $ \e p -> do
      end <- readIORef (peEnd e)
      if end `minusPtr` p >= n then
        pure $! p :!: ()
      else
        doGrow e p n
{-# INLINE grow #-}

runPut :: Put a -> ByteString
runPut = snd . evalPut
{-# INLINE runPut #-}

chunksLength :: [Chunk] -> Int
chunksLength = foldr (\c s -> s + chkEnd c `minusPtr` chkBegin c) 0
{-# INLINE chunksLength #-}

catChunks :: [Chunk] -> IO ByteString
catChunks chks = B.create (chunksLength chks) $ \p ->
  void $ foldlM (\q c -> do
                    let n = chkEnd c `minusPtr` chkBegin c
                    B.memcpy q (chkBegin c) n
                    free $ chkBegin c
                    pure (q `plusPtr` n)) p $ reverse chks
{-# INLINE catChunks #-}

evalPut :: Put a -> (a, ByteString)
evalPut p = unsafePerformIO $ do
  k <- newChunk 0
  chks <- newIORef (k:|[])
  end <- newIORef (chkEnd k)
  p' :!: r <- allocaBytes 8 $ \t ->
    unPut p PutEnv { peChks = chks, peEnd = end, peTmp = t } (chkBegin k)
  cs <- readIORef chks
  s <- case cs of
    (x:|xs) -> catChunks $ x { chkEnd = p' } : xs
  pure (r, s)
{-# NOINLINE evalPut #-}

putByteString :: ByteString -> Put ()
putByteString (B.PS b o n) = do
  grow n
  Put $ \_ p -> do
    withForeignPtr b $ \q -> B.memcpy p (q `plusPtr` o) n
    pure $! p `plusPtr` n :!: ()
{-# INLINE putByteString #-}
