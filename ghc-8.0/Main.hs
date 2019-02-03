-- <prana>
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
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
import           Data.Int
import qualified Constants as GHC
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Map as M
import           GHC.Exts (lazy)
import           GHC.Generics
import           Digraph (flattenSCC)
import           Data.Word
import qualified IdInfo as GHC
import           System.Directory (renameFile)
import qualified Data.Int
import           Data.Coerce (coerce)
import           Data.Bits
import qualified Data.ByteString.Unsafe as S
import           Control.Concurrent (threadDelay)
import           Data.Semigroup
import           Control.Arrow ((&&&))
import           Data.Function (on)
import           Control.Exception (IOException, bracket, catch)
import qualified DataCon as GHC
import qualified CorePrep as GHC
import qualified Type as GHC
import qualified TyCon as GHC
import qualified Var as GHC (isTyVar)
import qualified FastString as GHC
import           Data.Data
import           Data.String (fromString)
import qualified Unique as GHC
import           GHC.Real
import qualified InstEnv as GHC
import qualified Class as GHC
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Builder as L
import           Data.ByteString (ByteString)
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
import           CmdLineParser

-- Implementations of the various modes (--show-iface, mkdependHS. etc.)
import           LoadIface ( showIface )
import           HscMain ( newHscEnv )
import           DriverPipeline ( oneShot, compileFile )
import           DriverMkDepend ( doMkDependHS )
#ifdef GHCI
import           GHCi.UI ( interactiveUI, ghciWelcomeMsg, defaultGhciSettings )
#endif

-- Frontend plugins
#ifdef GHCI
import           DynamicLoading
import           Plugins
#else
import           DynamicLoading ( pluginError )
#endif
import           Module ( ModuleName )


-- Various other random stuff that we need
import           Config
import           Constants
import           HscTypes
import           Packages ( pprPackages, pprPackagesSimple, pprModuleMap )
import           DriverPhases
import           BasicTypes ( failed )
import           StaticFlags
import           DynFlags
import           ErrUtils
import           FastString
import           Outputable hiding ((<>))
import qualified Outputable ((<>))
import           SrcLoc
import           Util
import           Panic
import           UniqSupply
import           MonadUtils ( liftIO )

-- Imports for --abi-hash
import           LoadIface ( loadUserInterface )
import           Module ( mkModuleName )
import           Finder ( findImportedModule, cannotFindInterface )
import           TcRnMonad ( initIfaceCheck )
import           Binary ( openBinMem, put_, fingerprintBinMem )

-- Standard Haskell libraries
import           System.IO
import           System.Environment
import           System.Exit
import           System.FilePath
import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Maybe

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
    doPrana dflags'

doPrana dflags' = do
  mgraph <- GHC.getModuleGraph
  liftIO (hSetBuffering stdout NoBuffering)
  withIdDatabase
    (\(exportedIds0, localIds0, conIds0) -> do
       (exportedIds, localIds, conIds, ms) <-
         foldM
           (\(exportedIds00, localIds00, conIds00, modules0) modSummary -> do
              liftIO
                (putStr
                   ("Recompiling " ++
                    moduleToString (GHC.ms_mod modSummary) ++ " ... "))
              guts <- compile modSummary
              let bs = GHC.mg_binds guts
                  m = GHC.ms_mod modSummary
                  shallowIds = concatMap shallowBindingVars bs
                  deepIds = deepBindingVars m bs
                  exportedIds =
                    exportedIds00 ++
                    map (toExportedId m) (filter GHC.isExportedId shallowIds)
                  conIds =
                    concat
                      [ conIds00
                      , concatMap
                          (map (toConstrId m . GHC.dataConWorkId) . GHC.tyConDataCons)
                          (GHC.mg_tcs guts)
                      ]
                  localIds =
                    concat
                      [ localIds00
                      , map
                          (toLocalId m)
                          (filter (not . GHC.isExportedId) deepIds)
                      , map
                          (toLocalId m)
                          (filter (not . GHC.isExportedId) shallowIds)
                      ]
                  modules = modules0 ++ [(modSummary, guts)] -- Collect and generate afterwards.
              liftIO (putStrLn "done.")
              pure (exportedIds, localIds, conIds, modules))
           (exportedIds0, localIds0, conIds0, [])
           -- FIXME: This is a hack to avoid the compilation of Cabal's
           -- Setup.hs main package before we've built base, ghc-prim
           -- and integer-gmp.
           (filter
              (\modSummary ->
                 GHC.unpackFS
                   (GHC.unitIdFS (GHC.moduleUnitId (GHC.ms_mod modSummary))) /=
                 "main")
              mgraph)
       let !exportedMap = M.fromList (zip exportedIds [0 ..])
           !localMap = M.fromList (zip localIds [0 ..])
           !conMap = M.fromList (zip conIds [0 ..])
       mapM_
         (\(modSummary, guts) -> do
            liftIO
              (putStr
                 ("Writing " ++
                  moduleToFilePath (GHC.ms_mod modSummary) ++ " ... "))
            let m = GHC.ms_mod modSummary
                bs = GHC.mg_binds guts
                context =
                  Context
                    { contextModule = m
                    , contextExportedIds = exportedMap
                    , contextLocalIds = localMap
                    , contextConIds = conMap
                    , contextCore = Nothing
                    , contextDynFlags = dflags'
                    }
                binds = concatMap (toBinds context) bs
            liftIO
              (L.writeFile
                 (moduleToFilePath m)
                 (L.toLazyByteString (encodeArray (map encodeBind binds))))
            liftIO (putStrLn ("done.")))
         ms
       pure (exportedIds, localIds, conIds))

newtype OnSecond a b = OnSecond { getPair :: (a, b)}
instance Eq b => Eq (OnSecond a b) where (==) = on (==) (snd . getPair)
instance Ord b =>  Ord (OnSecond a b) where compare = on compare (snd . getPair)

deepBindingVars :: GHC.Module -> [CoreSyn.Bind GHC.Var] -> [GHC.Var]
deepBindingVars m =
  map (fst . getPair) . ordNub . map (OnSecond . (id &&& toLocalId m)) . concatMap getBindings
  where
    getBindings =
      concatMap
        (\case
           CoreSyn.Let bs _ -> shallowBindingVars bs
           CoreSyn.Lam var e -> [var]
           CoreSyn.Case _ var _ alts ->
             var : concatMap (\(_con, vars, e) -> vars) alts
           _ -> []) .
      listify (const True)

shallowBindingVars :: CoreSyn.Bind GHC.Var -> [GHC.Var]
shallowBindingVars =
  \case
    CoreSyn.NonRec i _ -> [i]
    CoreSyn.Rec rs -> map fst rs

-- isFromModule m _ = maybe True (== m) . GHC.nameModule_maybe . GHC.getName

withIdDatabase :: (([ExportedId], [LocalId], [ConstrId]) -> Ghc ([ExportedId],[LocalId],[ConstrId])) -> Ghc ()
withIdDatabase cont =
  GHC.gbracket
    (liftIO lock)
    (const (liftIO unlock))
    (\() -> do
       liftIO (putStrLn "Locked names database.")
       existing <- liftIO (S.readFile namesLocked)
       (exported, locals, cons) <- cont (decodeIds existing)
       liftIO (putStrLn "Writing names database ...")
       liftIO
         (L.writeFile
            namesReady
            (L.toLazyByteString (encodeExportedIds exported <> encodeLocalIds locals <> encodeConstrIds cons)))
       liftIO (renameFile namesReady namesLocked)
       liftIO (putStrLn "Updated and unlocked names database."))
  where
    lock =
      catch
        (renameFile namesUnlocked namesLocked)
        (\(_ :: IOException) -> do
           putStrLn ("Waiting for lock on " ++ namesUnlocked ++ " ...")
           threadDelay (1000 * 1000)
           lock)
    unlock = renameFile namesLocked namesUnlocked

namesLocked = namesUnlocked ++ ".lock"
namesUnlocked = "/root/prana/names-cache.db"
namesReady = namesLocked ++ ".new"

defaultConstrIds =
  [ ConstrId
      { constrIdPackage = "ghc-prim"
      , constrIdModule = "GHC.Prim"
      , constrIdName = "Unit#"
      }
  , ConstrId
      { constrIdPackage = "ghc-prim"
      , constrIdModule = "GHC.Types"
      , constrIdName = "Module"
      }
  , ConstrId
      { constrIdPackage = "ghc-prim"
      , constrIdModule = "GHC.Types"
      , constrIdName = "TrNameS"
      }
  ] ++
  [ ConstrId
    { constrIdPackage = "ghc-prim"
    , constrIdModule = "GHC.Prim"
    , constrIdName = "(#" <> S.replicate i (fromIntegral (fromEnum ',')) <> "#)"
    }
  | i <- [1 .. GHC.mAX_TUPLE_SIZE]
  ]

decodeIds :: ByteString -> ([ExportedId],[LocalId], [ConstrId])
decodeIds s0
  | S.null s0 = ([], [], defaultConstrIds)
  | otherwise = decodeExported (readInt s0) [] (S.drop 8 s0)
  where
    decodeExported 0 acc s =
      let (locals, cons) = decodeLocal (readInt s) [] (S.drop 8 s)
       in (reverse acc, locals, cons)
    decodeExported n acc s =
      let (pkg, s') = readShortByteString s
          (md, s'') = readShortByteString s'
          (name, s''') = readShortByteString s''
       in decodeExported
            (n - 1)
            (ExportedId
               { exportedIdPackage = pkg
               , exportedIdModule = md
               , exportedIdName = name
               } :
             acc)
            s'''
    decodeLocal 0 acc s = (reverse acc, decodeCons (readInt s) [] (S.drop 8 s))
    decodeLocal n acc s =
      let (pkg, s') = readShortByteString s
          (md, s'') = readShortByteString s'
          (name, s''') = readShortByteString s''
       in decodeLocal
            (n - 1)
            (LocalId
               { localIdPackage = pkg
               , localIdModule = md
               , localIdName = name
               , localIdUnique = Unique (readInt64 s''')
               } :
             acc)
            (S.drop 8 s''')
    decodeCons 0 acc _ =
      reverse acc
    decodeCons n acc s =
      let (pkg, s') = readShortByteString s
          (md, s'') = readShortByteString s'
          (name, s''') = readShortByteString s''
       in decodeCons
            (n - 1)
            (ConstrId
               {constrIdPackage = pkg, constrIdModule = md, constrIdName = name} :
             acc)
            s'''

encodeExportedIds :: [ExportedId] -> L.Builder
encodeExportedIds =
  encodeArray .
  map
    (\(ExportedId pkg md name) ->
       encodeShortByteString pkg <> encodeShortByteString md <>
       encodeShortByteString name)

encodeConstrIds :: [ConstrId] -> L.Builder
encodeConstrIds =
  encodeArray .
  map
    (\(ConstrId pkg md name) ->
       encodeShortByteString pkg <> encodeShortByteString md <>
       encodeShortByteString name)

encodeLocalIds :: [LocalId] -> L.Builder
encodeLocalIds =
  encodeArray .
  map
    (\(LocalId pkg md name uniq) ->
       encodeShortByteString pkg <> encodeShortByteString md <>
       encodeShortByteString name <>
       L.int64LE (unUnique uniq))

{-# INLINE readByteString #-}
-- | Read a ByteString from the encoding.
readByteString :: ByteString -> (ByteString, ByteString)
readByteString bs = (str, leftover)
  where str = S.unsafeTake (readInt bs) (S.unsafeDrop 8 bs)
        leftover = S.unsafeDrop (readInt bs + 8) bs

-- | Read a ByteString from the encoding.
readShortByteString :: ByteString -> (ByteString, ByteString)
readShortByteString bs = (str, leftover)
  where str = S.unsafeTake (fromIntegral (readInt16 bs)) (S.unsafeDrop 2 bs)
        leftover = S.unsafeDrop (fromIntegral (readInt16 bs + 2)) bs

{-# INLINE readInt16 #-}
readInt16 :: ByteString -> Data.Int.Int16
readInt16 = fromIntegral . readWord16le

{-# INLINE readInt #-}
readInt :: ByteString -> Int
readInt = fromIntegral . readInt64

{-# INLINE readInt64 #-}
readInt64 :: ByteString -> Data.Int.Int64
readInt64 = fromIntegral . readWord64

readWord16le :: ByteString -> Word16
readWord16le = \s ->
              (fromIntegral (s `S.unsafeIndex` 1) `unsafeShiftL` 8) .|.
              (fromIntegral (s `S.unsafeIndex` 0) )

{-# INLINE readWord64 #-}
readWord64 :: ByteString -> Word64
readWord64 s =
  x7 `unsafeShiftL` 56 .|. x6 `unsafeShiftL` 48 .|. x5 `unsafeShiftL` 40 .|.
  x4 `unsafeShiftL` 32 .|.
  x3 `unsafeShiftL` 24 .|.
  x2 `unsafeShiftL` 16 .|.
  x1 `unsafeShiftL` 8 .|.
  x0
  where
    x0 = fromIntegral (S.unsafeIndex s 0) :: Word64
    x1 = fromIntegral (S.unsafeIndex s 1) :: Word64
    x2 = fromIntegral (S.unsafeIndex s 2) :: Word64
    x3 = fromIntegral (S.unsafeIndex s 3) :: Word64
    x4 = fromIntegral (S.unsafeIndex s 4) :: Word64
    x5 = fromIntegral (S.unsafeIndex s 5) :: Word64
    x6 = fromIntegral (S.unsafeIndex s 6) :: Word64
    x7 = fromIntegral (S.unsafeIndex s 7) :: Word64

data Context =
  Context { contextModule :: GHC.Module
          , contextExportedIds :: M.Map ExportedId Int64
          , contextLocalIds :: M.Map LocalId Int64
          , contextCore :: Maybe (CoreSyn.Expr GHC.Var)
          , contextDynFlags :: GHC.DynFlags
          , contextConIds :: M.Map ConstrId Int64
          }

toBinds :: Context -> CoreSyn.Bind GHC.Var -> [Main.Bind]
toBinds m =
  \case
    CoreSyn.NonRec v e ->
      [ Bind
          (toVarId m v)
          (either
             (\eee ->
                error
                  (eee ++
                   ", core was: " ++
                   L8.unpack (L.toLazyByteString (showExpr True e))))
             id
             (toExp m {contextCore = Just e} e))
      ]
    CoreSyn.Rec bs ->
      map
        (\(v, e) ->
           Bind
             (toVarId m v)
             (either
                (\eee ->
                   error
                     (eee ++
                      ", core was: " ++
                      L8.unpack (L.toLazyByteString (showExpr True e))))
                id
                (toExp m {contextCore = Just e} e)))
        bs


toLocalBinds :: Context -> CoreSyn.Bind GHC.Var -> [(Main.LocalVarId, Main.Exp)]
toLocalBinds m =
  \case
    CoreSyn.NonRec v e ->
      [
          (toLocalVarId m v,
          either
             (\eee ->
                error
                  (eee ++
                   ", core was: " ++
                   L8.unpack (L.toLazyByteString (showExpr True e))))
             id
             (toExp m {contextCore = Just e} e))
      ]
    CoreSyn.Rec bs ->
      map
        (\(v, e) ->

             (toLocalVarId m v
             ,either
                 (\eee ->
                    error
                      (eee ++
                       ", core was: " ++
                       L8.unpack (L.toLazyByteString (showExpr True e))))
                 id
                 (toExp m {contextCore = Just e} e)))
        bs

toExp :: Context -> CoreSyn.Expr GHC.Var -> Either String Main.Exp
toExp m =
  \case
    CoreSyn.App f (CoreSyn.Type _) -> toExp m f
    -- ^ v Skip over type and coerce applications.
    CoreSyn.App f (CoreSyn.Coercion _) -> toExp m f
    CoreSyn.App f x -> Main.AppE <$> toExp m f <*> toExp m x
    CoreSyn.Lam var body ->
      if GHC.isTyVar var -- Skip lambdas of types.
        then toExp m body
        else Main.LamE <$> pure (toLocalVarId m var) <*> toExp m body
    CoreSyn.Let bind expr ->
      Main.LetE <$> pure (toLocalBinds m bind) <*> toExp m expr
    CoreSyn.Case expr var typ alts ->
      Main.CaseE <$> toExp m expr <*> pure (toVarId m var) <*>
      pure (toTyp m typ) <*>
      (mapM (toAlt m) alts)
    CoreSyn.Var i -> pure (toSomeIdExp m i)
    CoreSyn.Lit i -> pure (Main.LitE (toLit i))
    CoreSyn.Cast expr _coercion -> toExp m expr
    CoreSyn.Tick _tickishVar expr -> toExp m expr
    -- Types should only appear in argument position.
    CoreSyn.Type typ -> Left "Did not expect a type here!"
    -- Coercions should only appear in an argument position.
    CoreSyn.Coercion _coercion -> Left "Did not expect a coercion here!"

isNewtype' i =
  case GHC.isDataConId_maybe i of
    Nothing -> False
    Just dc -> GHC.isNewTyCon (GHC.dataConTyCon dc)

toAlt :: Context -> (CoreSyn.AltCon, [GHC.Var], CoreSyn.Expr GHC.Var) -> Either String Alt
toAlt m (con,vars,e) = Alt <$> pure (toAltCon m con) <*> pure (map (toVarId m) vars) <*> (toExp m e)

toAltCon :: Context -> CoreSyn.AltCon -> Main.AltCon
toAltCon m =
  \case
    CoreSyn.DataAlt dataCon -> DataAlt (toDataCon m dataCon)
    CoreSyn.LitAlt literal  -> LitAlt (toLit literal)
    CoreSyn.DEFAULT         -> DEFAULT

toLit :: GHC.Literal -> Main.Lit
toLit =
  \case
    GHC.MachChar i          -> Char i
    GHC.MachStr i           -> Str i
    GHC.MachNullAddr        -> NullAddr
    GHC.MachInt i           -> Int i
    GHC.MachInt64 i         -> Int64Lit i
    GHC.MachWord i          -> Word i
    GHC.MachWord64 i        -> Word64 i
    GHC.MachFloat (i)  -> Float i
    GHC.MachDouble (i) -> Double i
    GHC.MachLabel _ _ _     -> Label
    GHC.LitInteger i _typ   -> Integer i

toTyp :: Context -> GHC.Type -> Main.Typ
toTyp m t =
  case GHC.splitTyConApp_maybe t of
    Nothing -> Main.OpaqueType (S8.pack (GHC.showSDocUnsafe (GHC.ppr t)))
    Just (tyCon, tys) ->
      Main.TyConApp (toTyId m (GHC.tyConName tyCon)) (map (toTyp m) tys)

toDataCon :: Context -> GHC.DataCon -> Main.DataCon
toDataCon m dc =
  Main.DataCon
    (toConId m (GHC.dataConName dc))
    (map toStrictness (GHC.dataConImplBangs dc))
  where
    toStrictness =
      \case
        GHC.HsLazy -> NonStrict
        GHC.HsStrict -> Strict
        GHC.HsUnpack mc -> Strict

toConId :: GHC.NamedThing thing => Context -> thing -> Main.ConId
toConId m var =
  case M.lookup i0 (contextConIds m) of
    Just idx -> ConId idx
    Nothing ->
      error
        (unlines
           [ "Not in constructor scope: " ++
             show (toConstrId (contextModule m) var)
           , "I have these in that module:"
           , unlines
               (map
                  show
                  (filter
                     (\i ->
                        constrIdPackage i == constrIdPackage i0 &&
                        constrIdModule i == constrIdModule i0)
                     (M.keys (contextConIds m))))
           , "Expression:"
           , maybe
               ""
               (showSDoc (contextDynFlags m) . ppr)
               (contextCore m)
           ,"AKA:"
           ,maybe
                ""
                (L8.unpack . L.toLazyByteString . showExpr False)
                (contextCore m)
           ])
  where
    i0 = toConstrId (contextModule m) var

toTyId :: GHC.NamedThing thing => Context -> thing -> Main.TyId
toTyId _ _ = Main.TyId

toSomeIdExp :: Context -> GHC.Var -> Main.Exp
toSomeIdExp m (wrapperToWorker -> var) =
  if isNewtype' var
    then Main.LamE (LocalVarId 0) (VarE (LocalIndex 0)) -- identity.
    else case GHC.isDataConWorkId_maybe var of
           Just dataCon -> Main.ConE (toConId m var)
           Nothing ->
             case GHC.isPrimOpId_maybe var of
               Just primOp -> Main.PrimOpE Main.PrimId
               Nothing ->
                 case GHC.wiredInNameTyThing_maybe (GHC.getName var) of
                   Just wiredIn -> Main.WiredInE Main.WiredId
                   Nothing ->
                     case GHC.isClassOpId_maybe var of
                       Just cls -> Main.MethodE Main.MethodId
                       Nothing ->
                         case GHC.isFCallId_maybe var of
                           Just fcall -> Main.FFIE Main.FFIId
                           Nothing ->
                             case GHC.isDictId var of
                               True -> Main.DictE Main.DictId
                               False -> Main.VarE (toVarId m var)

-- | We don't support wrappers, so we convert all wrappers to workers.
wrapperToWorker :: GHC.Id -> GHC.Id
wrapperToWorker i =
  case GHC.idDetails i of
    GHC.DataConWrapId dataCon -> GHC.dataConWorkId dataCon
    _ -> i

toVarId :: Context -> GHC.Var -> Main.VarId
toVarId m var =
  if GHC.isExportedId var
    then case M.lookup i0 (contextExportedIds m) of
           Just idx -> ExportedIndex idx
           Nothing ->
             error
               (unlines
                  [ "Not in exported scope: " ++
                    show (toExportedId (contextModule m) var)
                  , "I have these in that module:"
                  , unlines
                      (map
                         show
                         (filter
                            (\i ->
                               exportedIdPackage i == exportedIdPackage i0 &&
                               exportedIdModule i == exportedIdModule i0)
                            (M.keys (contextExportedIds m))))
                  , "Expression:"
                  , maybe
                      ""
                      (showSDoc (contextDynFlags m) . ppr)
                      (contextCore m)
                  ,"AKA:"
                  ,maybe
                       ""
                       (L8.unpack . L.toLazyByteString . showExpr False)
                       (contextCore m)
                  ])
    else case M.lookup i1 (contextLocalIds m) of
           Just idx -> LocalIndex idx
           Nothing ->
             error
               (unlines
                  [ "Not in local scope: " ++
                    show (toLocalId (contextModule m) var)
                  , "I have these in that module:"
                  , unlines
                      (map
                         show
                         (filter
                            (\i ->
                               localIdPackage i == localIdPackage i1 &&
                               localIdModule i == localIdModule i1)
                            (M.keys (contextLocalIds m))))
                  , "Expression:"
                  , maybe
                      ""
                      (showSDoc (contextDynFlags m) . ppr)
                      (contextCore m)
                  ,"AKA:"
                  ,maybe
                       ""
                       (L8.unpack . L.toLazyByteString . showExpr False)
                       (contextCore m)
                  ])
  where
    i0 = toExportedId (contextModule m) var
    i1 = toLocalId (contextModule m) var

toLocalVarId m var =
  case M.lookup i1 (contextLocalIds m) of
    Just idx -> LocalVarId idx
    Nothing ->
      error
        (unlines
           [ "Not in local scope: " ++ show (toLocalId (contextModule m) var)
           , "I have these in that module:"
           , unlines
               (map
                  show
                  (filter
                     (\i ->
                        localIdPackage i == localIdPackage i1 &&
                        localIdModule i == localIdModule i1)
                     (M.keys (contextLocalIds m))))
           , "Expression:"
           , maybe "" (showSDoc (contextDynFlags m) . ppr) (contextCore m)
           , "AKA:"
           , maybe
               ""
               (L8.unpack . L.toLazyByteString . showExpr False)
               (contextCore m)
           ])
  where i1 = toLocalId (contextModule m) var

toExportedId :: GHC.NamedThing thing => GHC.Module -> thing -> Main.ExportedId
toExportedId m thing =
  if GHC.isInternalName name
    then error "toExportedId: internal name given"
    else Main.ExportedId package module' name'
  where
    package = GHC.fs_bs (GHC.unitIdFS (GHC.moduleUnitId (GHC.nameModule name)))
    module' =
      GHC.fs_bs (GHC.moduleNameFS (GHC.moduleName (GHC.nameModule name)))
    name' = GHC.fs_bs (GHC.getOccFS name)
    name = case GHC.nameModule_maybe n of
             Nothing -> qualify m n
             Just{} -> n
          where n = GHC.getName thing

toConstrId :: GHC.NamedThing thing => GHC.Module -> thing -> Main.ConstrId
toConstrId m thing =
  if GHC.isInternalName name
    then error "toConstrId: internal name given"
    else Main.ConstrId package module' name'
  where
    package = GHC.fs_bs (GHC.unitIdFS (GHC.moduleUnitId (GHC.nameModule name)))
    module' =
      GHC.fs_bs (GHC.moduleNameFS (GHC.moduleName (GHC.nameModule name)))
    name' = GHC.fs_bs (GHC.getOccFS name)
    name = case GHC.nameModule_maybe n of
             Nothing -> qualify m n
             Just{} -> n
          where n = GHC.getName thing

toLocalId :: GHC.NamedThing thing => GHC.Module -> thing -> Main.LocalId
toLocalId m thing =
  if GHC.isInternalName name
    then error "toLocalId: internal name given"
    else Main.LocalId package module' name' (Main.Unique (fromIntegral (GHC.getKey (GHC.getUnique name))))
  where
    package = GHC.fs_bs (GHC.unitIdFS (GHC.moduleUnitId (GHC.nameModule name)))
    module' =
      GHC.fs_bs (GHC.moduleNameFS (GHC.moduleName (GHC.nameModule name)))
    name' = GHC.fs_bs (GHC.getOccFS name)
    name = case GHC.nameModule_maybe n of
             Nothing -> qualify m n
             Just{} -> n
          where n = GHC.getName thing



qualify :: GHC.Module -> GHC.Name -> GHC.Name
qualify m name =
  GHC.mkExternalName
    (GHC.getUnique name)
    m
    (GHC.nameOccName name)
    (GHC.nameSrcSpan name)

qualifiedNameByteString :: GHC.Name -> (ByteString,Cat)
qualifiedNameByteString n =
  case GHC.nameModule_maybe n of
    Nothing -> (sort' <> ":" <> ident, ValCat)
      where sort' =
              if GHC.isInternalName n
                then "internal"
                else if GHC.isSystemName n
                       then "system"
                       else "unknown"
    Just mo ->
      ( package <> ":" <> module' <> "." <> ident
      , if S.isPrefixOf "C:" ident
          then ClassCat
          else if S8.all isUpper (S.take 1 ident)
                 then DataCat
                 else if S8.all (\c -> c=='(' || c==')' || c==',') ident
                         then DataCat
                         else ValCat)
      where package = GHC.fs_bs (GHC.unitIdFS (GHC.moduleUnitId mo))
            module' = GHC.fs_bs (GHC.moduleNameFS (GHC.moduleName mo))
  where
    ident = GHC.fs_bs (GHC.getOccFS n)

compile ::
     GHC.GhcMonad m
  => GHC.ModSummary
  -> m GHC.ModGuts
compile modSummary = do
  parsedModule <- GHC.parseModule modSummary
  typecheckedModule <- GHC.typecheckModule parsedModule
  desugared <- GHC.desugarModule typecheckedModule
  pure (GHC.dm_core_module desugared)

moduleToString :: GHC.Module -> String
moduleToString module' = moduleNameString
  where
    unitId = GHC.moduleUnitId module'
    moduleName_ = GHC.moduleName module'
    packageNameVersion = GHC.unitIdString unitId
    moduleNameString = GHC.moduleNameString moduleName_

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
-- SYB

type GenericQ r = forall a. Data a => a -> r
everything :: (r -> r -> r) -> GenericQ r -> GenericQ r
everything k f x = foldl k (f x) (gmapQ (everything k f) x)
listify :: Typeable r => (r -> Bool) -> GenericQ [r]
listify p = everything (++) ([] `mkQ` (\x -> if p x then [x] else []))
mkQ :: ( Typeable a , Typeable b) => r -> (b -> r) -> a -> r
(r `mkQ` br) a = case cast a of
                        Just b  -> br b
                        Nothing -> r

--------------------------------------------------------------------------------
-- Binary writing

encodeBind :: Main.Bind -> L.Builder
encodeBind =
  \case
    Main.Bind var expr -> encodeId var <> encodeExpr expr

encodeLocalBind :: (Main.LocalVarId, Main.Exp) -> L.Builder
encodeLocalBind =
  \case
    (var, expr) -> encodeLocalVarId var <> encodeExpr expr

encodeExpr :: Main.Exp -> L.Builder
encodeExpr =
  \case
    Main.VarE i -> tag 0 <> encodeId i
    Main.LitE i -> tag 1 <> encodeLit i
    Main.AppE f x -> tag 2 <> encodeExpr f <> encodeExpr x
    Main.LamE var body -> tag 3 <> encodeLocalVarId var <> encodeExpr body
    Main.LetE binds expr ->
      tag 4 <> encodeArray (map encodeLocalBind binds) <> encodeExpr expr
    Main.CaseE expr var typ alts ->
      tag 5 <> encodeExpr expr <> encodeId var <> encodeType typ <>
      encodeArray (map encodeAlt alts)
    Main.ConE i -> tag 10 <> encodeConId i
    Main.PrimOpE i -> tag 11 <> encodePrimId i
    Main.WiredInE i -> tag 12 <> encodeWiredIn i
    Main.MethodE i -> tag 13 <> encodeMethodId i
    Main.DictE i -> tag 14 <> encodeDictId i
    Main.FFIE i -> tag 15 <> encodeFFIId i

encodeLit :: Main.Lit -> L.Builder
encodeLit =
  \case
    Char i          -> tag 0 <> encodeChar i
    Str i           -> tag 1 <> encodeByteString i
    NullAddr        -> tag 2
    Int i           -> tag 3 <> encodeInteger i
    Int64Lit i         -> tag 4 <> encodeInteger i
    Word i          -> tag 5 <> encodeInteger i
    Word64 i        -> tag 6 <> encodeInteger i
    Float (i :% j)  -> tag 7 <> encodeInteger i <> encodeInteger j
    Double (i :% j) -> tag 8 <> encodeInteger i <> encodeInteger j
    Label           -> tag 9
    Integer i       -> tag 10 <> encodeInteger i

encodeInteger :: Integer -> L.Builder
encodeInteger = encodeLazyByteString . L.toLazyByteString . L.integerDec

encodeInt :: Int -> L.Builder
encodeInt = L.int64LE . fromIntegral

encodeBool :: Bool -> L.Builder
encodeBool =
  \case
    True -> tag 1
    False -> tag 0

encodeAltCon :: AltCon -> L.Builder
encodeAltCon =
  \case
    DataAlt dataCon -> tag 0 <> encodeDataCon dataCon
    LitAlt literal  -> tag 1 <> encodeLit literal
    DEFAULT         -> tag 2

encodeAlt :: Alt -> L.Builder
encodeAlt (Alt altCon' vars expr) =
  encodeAltCon altCon' <> encodeArray (map encodeId vars) <> encodeExpr expr

tag :: Word8 -> L.Builder
tag = L.word8

encodeType :: Typ -> L.Builder
encodeType =
  \case
    OpaqueType e -> tag 0 <> encodeByteString e
    TyConApp i es -> tag 1 <> encodeTyId i <> encodeArray (map encodeType es)

encodeId :: VarId -> L.Builder
encodeId (LocalIndex x) = tag 0 <> L.int64LE x
encodeId (ExportedIndex x) = tag 1 <> L.int64LE x

encodeLocalVarId :: LocalVarId -> L.Builder
encodeLocalVarId (LocalVarId x) = L.int64LE x

encodeConId :: ConId -> L.Builder
encodeConId (ConId i) = L.int64LE i

encodeMethodId :: MethodId -> L.Builder
encodeMethodId _ = mempty

encodeDictId :: DictId -> L.Builder
encodeDictId _ = mempty

encodeFFIId :: FFIId -> L.Builder
encodeFFIId _ = mempty

encodePrimId :: PrimId -> L.Builder
encodePrimId _ = mempty

encodeWiredIn :: WiredId -> L.Builder
encodeWiredIn _ = mempty

encodeTyId :: TyId -> L.Builder
encodeTyId _ = mempty

encodeCat :: Cat -> L.Builder
encodeCat =
  L.word8 .
  (\case
     ValCat -> 0
     DataCat -> 1
     ClassCat -> 2)

encodeDataCon :: DataCon -> L.Builder
encodeDataCon (DataCon e ss) = encodeConId e <> encodeArray (map encodeStrictness ss)

encodeStrictness :: Strictness -> L.Builder
encodeStrictness =
  \case
    Strict -> L.word8 0
    NonStrict -> L.word8 1

encodeUnique :: Unique -> L.Builder
encodeUnique (Unique x) = L.int64LE x

encodeByteString :: ByteString -> L.Builder
encodeByteString x =
  L.int64LE (fromIntegral (S.length x)) <>
  L.byteString x

encodeShortByteString :: ByteString -> L.Builder
encodeShortByteString x =
  L.int16LE (fromIntegral (S.length x)) <>
  L.byteString x

encodeLazyByteString :: L.ByteString -> L.Builder
encodeLazyByteString x =
  L.int64LE (fromIntegral (L.length x)) <>
  L.lazyByteString x

encodeChar :: Char -> L.Builder
encodeChar = L.int64LE . fromIntegral . fromEnum

encodeArray :: [L.Builder] -> L.Builder
encodeArray v = L.int64LE (fromIntegral (length v)) <> mconcat v

--------------------------------------------------------------------------------
-- Extra utils

-- We define 'ordNub' here to give GHC as many opportunities to
-- optimize as possible.
ordNub :: Ord a => [a] -> [a]
ordNub = go Set.empty
  where
    go _ [] = []
    go s (x : xs) =
      if Set.member x s
        then go s xs
        else x : go (Set.insert x s) xs

--------------------------------------------------------------------------------
-- Pretty core


showBind :: Bool -> CoreSyn.Bind GHC.Var -> L.Builder
showBind ps =
  \case
    CoreSyn.NonRec var expr ->
      par ps ("NonRec " <> showVar True var <> " " <> showExpr True expr)
    CoreSyn.Rec exprs ->
      par
        ps
        ("Rec [" <>
         mconcat (intersperse
                    ","
                    (map
                       (\(x, y) ->
                          "(" <> showVar False x <> ", " <> showExpr False y <>
                          ")")
                       exprs)) <>
         "]")

showVar :: Bool -> GHC.Var -> L.Builder
showVar ps v = par ps ("Var " <> showL (GHC.nameStableString (GHC.getName v)))

showId :: Bool -> GHC.Id -> L.Builder
showId ps v = par ps ("Id " <> showL (GHC.nameStableString (GHC.getName v)))

showExpr :: Bool -> CoreSyn.Expr GHC.Var -> L.Builder
showExpr ps =
  \case
    CoreSyn.Var vid -> par ps ("Var " <> showId ps vid)
    CoreSyn.Lit literal -> par ps ("Lit " <> showLiteral True literal)
    CoreSyn.App f x ->
      par ps ("App " <> showExpr True f <> " " <> showExpr True x)
    CoreSyn.Lam var body ->
      par ps ("Lam " <> showVar True var <> " " <> showExpr True body)
    CoreSyn.Let bind expr ->
      par ps ("Let " <> (showBind True bind) <> " " <> showExpr True expr)
    CoreSyn.Case expr var typ alts ->
      par
        ps
        ("Case " <> showExpr True expr <> " " <> showVar True var <> " " <>
         showType typ <>
         " [" <>
         mconcat (intersperse ", " (map showAlt alts)) <>
         "] ")
    CoreSyn.Cast expr coercion -> par ps ("Cast " <> showExpr True expr)
    CoreSyn.Tick tickishVar expr -> par ps ("Tick " <> showExpr True expr)
    CoreSyn.Type typ -> "Type"
    CoreSyn.Coercion coercion -> "Coercion"

showAlt :: (CoreSyn.AltCon, [GHC.Var], CoreSyn.Expr GHC.Var) -> L.Builder
showAlt (con, vs, e) =
  "(" <> showAltCon con <> ", [" <> mconcat (intersperse ", " (map (showVar False) vs)) <>
  "], " <>
  showExpr False e <>
  ")"

showAltCon :: CoreSyn.AltCon -> L.Builder
showAltCon =
  \case
    CoreSyn.DataAlt dataCon -> fromString "DataAlt"
    CoreSyn.LitAlt literal  -> fromString "LitAlt"
    CoreSyn.DEFAULT         -> fromString "DEFAULT"

showLiteral :: Bool -> GHC.Literal -> L.Builder
showLiteral ps =
  \case
    GHC.MachChar ch -> par ps ("MachChar " <> showL ch)
    GHC.MachStr str -> par ps ("MachStr " <> showL str)
    GHC.MachNullAddr -> "MachNullAddr"
    GHC.MachInt i -> par ps ("MachInt " <> showL i)
    GHC.MachInt64 i -> par ps ("MachInt64 " <> showL i)
    GHC.MachWord i -> par ps ("MachWord " <> showL i)
    GHC.MachWord64 i -> par ps ("MachWord64 " <> showL i)
    GHC.MachFloat i -> par ps ("MachFloat " <> showL i)
    GHC.MachDouble i -> par ps ("MachDouble " <> showL i)
    GHC.MachLabel fastString mint functionOrData -> par ps ("MachLabel ")
    GHC.LitInteger i typ -> par ps ("LitInteger " <> showL i <> " " <> showType typ)

showL :: Show i => i -> L.Builder
showL i = fromString (show i)

showType :: GHC.Type -> L.Builder
showType _ = "Type"

par :: Bool -> L.Builder -> L.Builder
par True x = "(" <> x <> ")"
par _    x = x
