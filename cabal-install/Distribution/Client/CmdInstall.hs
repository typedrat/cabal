{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

-- | cabal-install CLI command: build
--
module Distribution.Client.CmdInstall (
    -- * The @build@ CLI and action
    installCommand,
    installAction,

    -- * Internals exposed for testing
    TargetProblem(..),
    selectPackageTargets,
    selectComponentTarget,
    establishDummyProjectBaseContext
  ) where

import Prelude ()
import Distribution.Client.Compat.Prelude
import Distribution.Compat.Directory
         ( doesPathExist )

import Distribution.Client.ProjectOrchestration
import Distribution.Client.CmdErrorMessages
import Distribution.Client.CmdSdist

import Distribution.Client.CmdInstall.ClientInstallFlags
import Distribution.Client.CmdInstall.InstallTarget

import Distribution.Client.Setup
         ( GlobalFlags(..), ConfigFlags(..), ConfigExFlags, InstallFlags(..)
         , configureExOptions, haddockOptions, installOptions, testOptions
         , configureOptions, liftOptions )
import Distribution.Client.GlobalFlags
         ( RepoContext(..) )
import Distribution.Solver.Types.ConstraintSource
         ( ConstraintSource(..) )
import Distribution.Client.Types
         ( PackageSpecifier(..), PackageLocation(..), UnresolvedSourcePackage
         , SourcePackageDb(..), Unresolved, isRepoRemote )

import Distribution.Compat.Lens
import qualified Distribution.Types.Lens as L

import Distribution.Types.BuildInfo
         ( BuildInfo(..), emptyBuildInfo )
import Distribution.Types.CondTree
         ( CondTree(..), traverseCondTreeC )
import Distribution.Types.ExeDependency
         ( ExeDependency(..) )
import Distribution.Types.GenericPackageDescription
         ( GenericPackageDescription, emptyGenericPackageDescription )
import Distribution.Types.Library
         ( Library(..), emptyLibrary )
import Distribution.Types.LibraryVisibility
         ( LibraryVisibility(..) )
import Distribution.Types.Executable
         ( Executable(..) )
import Distribution.Types.ForeignLib
         ( ForeignLib(..) )
import Distribution.Types.TestSuite
         ( TestSuite(..) )
import Distribution.Types.Benchmark
         ( Benchmark(..) )
import Distribution.Types.PackageDescription
         ( PackageDescription(..), emptyPackageDescription, allLibraries )
import Distribution.PackageDescription.Configuration
         ( flattenPackageDescription )
import Distribution.PackageDescription.PrettyPrint
import Distribution.Types.PackageName.Magic
         ( fakePackageId )
import qualified Distribution.SPDX.License as SPDX
import Language.Haskell.Extension
         ( Language(..) )

import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Package
         ( Dependency(..), Package(..), PackageName, mkPackageName, unPackageName )
import Distribution.Types.PackageId
         ( PackageIdentifier(..) )
import Distribution.Client.ProjectConfig.Types
         ( ProjectConfig(..), ProjectConfigShared(..)
         , ProjectConfigBuildOnly(..), PackageConfig(..)
         , getMapLast, getMapMappend, projectConfigLogsDir
         , projectConfigStoreDir, projectConfigBuildOnly
         , projectConfigDistDir, projectConfigConfigFile )
import Distribution.Simple.Program.Db
         ( userSpecifyPaths, userSpecifyArgss, defaultProgramDb
         , modifyProgramSearchPath, ProgramDb )
import Distribution.Simple.BuildPaths
         ( exeExtension )
import Distribution.Simple.Program.Find
         ( ProgramSearchPathEntry(..) )
import Distribution.Client.Config
         ( getCabalDir, loadConfig, SavedConfig(..) )
import qualified Distribution.Simple.PackageIndex as PI
import qualified Distribution.Solver.Types.PackageIndex as SPI
import Distribution.Solver.Types.PackageIndex
         ( lookupPackageName, searchByName )
import Distribution.Types.InstalledPackageInfo
         ( InstalledPackageInfo( InstalledPackageInfo, sourcePackageId, sourceLibName ) )
import Distribution.Types.Version
         ( mkVersion, nullVersion )
import Distribution.Types.VersionRange
         ( thisVersion )
import Distribution.Solver.Types.PackageConstraint
         ( PackageProperty(..) )
import Distribution.Client.IndexUtils
         ( getSourcePackages, getInstalledPackages )
import Distribution.Client.ProjectConfig
         ( readGlobalConfig, projectConfigWithSolverRepoContext
         , resolveBuildTimeSettings, withProjectOrGlobalConfig )
import Distribution.Client.ProjectPlanning
         ( storePackageInstallDirs' )
import qualified Distribution.Simple.InstallDirs as InstallDirs
import Distribution.Client.DistDirLayout
         ( defaultDistDirLayout, DistDirLayout(..), mkCabalDirLayout
         , ProjectRoot(ProjectRootImplicit)
         , cabalStoreDirLayout
         , CabalDirLayout(..), StoreDirLayout(..) )
import Distribution.Client.RebuildMonad
         ( runRebuild )
import Distribution.Client.InstallSymlink
         ( OverwritePolicy(..), symlinkBinary )
import Distribution.Simple.Setup
         ( Flag(..), HaddockFlags, TestFlags, fromFlagOrDefault, flagToMaybe )
import Distribution.Solver.Types.SourcePackage
         ( SourcePackage(..) )
import Distribution.Simple.Command
         ( CommandUI(..), OptionField(..), usageAlternatives )
import Distribution.Simple.Configure
         ( configCompilerEx )
import Distribution.Simple.Compiler
         ( Compiler(..), CompilerId(..), CompilerFlavor(..)
         , PackageDB(..), PackageDBStack )
import Distribution.Simple.GHC
         ( ghcPlatformAndVersionString
         , GhcImplInfo(..), getImplInfo
         , GhcEnvironmentFileEntry(..)
         , renderGhcEnvironmentFile, readGhcEnvironmentFile, ParseErrorExc )
import Distribution.System
         ( Platform )
import Distribution.Types.UnitId
         ( UnitId )
import Distribution.Types.ComponentName
         ( ComponentName(..), componentNameString )
import Distribution.Types.UnqualComponentName
         ( UnqualComponentName, unUnqualComponentName, mkUnqualComponentName )
import Distribution.Verbosity
         ( Verbosity, normal, lessVerbose )
import Distribution.Simple.Utils
         ( wrapText, die', notice, warn
         , withTempDirectory, createDirectoryIfMissingVerbose
         , ordNub )
import Distribution.Utils.Generic
         ( writeFileAtomic )
import Distribution.Deprecated.Text
         ( simpleParse )
import Distribution.Parsec
import Distribution.Parsec.FieldLineStream 
         ( fieldLineStreamFromString )
import Distribution.Pretty
         ( prettyShow )

import Control.Exception
         ( catch )
import Control.Monad
         ( mapM, mapM_ )
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Either
         ( partitionEithers )
import Data.Monoid
         ( First(..) )
import Data.Ord
         ( comparing, Down(..) )
import qualified Data.Map as Map
import qualified Data.Set as Set
import Distribution.Utils.NubList
         ( fromNubList )
import System.Directory
         ( getHomeDirectory, doesFileExist, createDirectoryIfMissing
         , getTemporaryDirectory, makeAbsolute, doesDirectoryExist
         , removeFile, removeDirectory, copyFile )
import System.FilePath
         ( (</>), (<.>), takeDirectory, takeBaseName )


installCommand :: CommandUI ( ConfigFlags, ConfigExFlags, InstallFlags
                            , HaddockFlags, TestFlags, ClientInstallFlags
                            )
installCommand = CommandUI
  { commandName         = "v2-install"
  , commandSynopsis     = "Install packages."
  , commandUsage        = usageAlternatives
                          "v2-install" [ "[TARGETS] [FLAGS]" ]
  , commandDescription  = Just $ \_ -> wrapText $
    "Installs one or more packages. This is done by installing them "
    ++ "in the store and symlinking/copying the executables in the directory "
    ++ "specified by the --installdir flag (`~/.cabal/bin/` by default). "
    ++ "If you want the installed executables to be available globally, "
    ++ "make sure that the PATH environment variable contains that directory. "
    ++ "\n\n"
    ++ "If TARGET is a library, it will be added to the global environment. "
    ++ "When doing this, cabal will try to build a plan that includes all "
    ++ "the previously installed libraries. This is currently not implemented."
  , commandNotes        = Just $ \pname ->
      "Examples:\n"
      ++ "  " ++ pname ++ " v2-install\n"
      ++ "    Install the package in the current directory\n"
      ++ "  " ++ pname ++ " v2-install pkgname\n"
      ++ "    Install the package named pkgname"
      ++ " (fetching it from hackage if necessary)\n"
      ++ "  " ++ pname ++ " v2-install ./pkgfoo\n"
      ++ "    Install the package in the ./pkgfoo directory\n"

      ++ cmdCommonHelpTextNewBuildBeta
  , commandOptions      = \showOrParseArgs ->
        liftOptions get1 set1
        -- Note: [Hidden Flags]
        -- hide "constraint", "dependency", and
        -- "exact-configuration" from the configure options.
        (filter ((`notElem` ["constraint", "dependency"
                            , "exact-configuration"])
                 . optionName) $ configureOptions showOrParseArgs)
     ++ liftOptions get2 set2 (configureExOptions showOrParseArgs
                               ConstraintSourceCommandlineFlag)
     ++ liftOptions get3 set3
        -- hide "target-package-db" and "symlink-bindir" flags from the
        -- install options.
        -- "symlink-bindir" is obsoleted by "installdir" in ClientInstallFlags
        (filter ((`notElem` ["target-package-db", "symlink-bindir"])
                 . optionName) $
                               installOptions showOrParseArgs)
       ++ liftOptions get4 set4
          -- hide "verbose" and "builddir" flags from the
          -- haddock options.
          (filter ((`notElem` ["v", "verbose", "builddir"])
                  . optionName) $
                                haddockOptions showOrParseArgs)
     ++ liftOptions get5 set5 (testOptions showOrParseArgs)
     ++ liftOptions get6 set6 (clientInstallOptions showOrParseArgs)
  , commandDefaultFlags = ( mempty, mempty, mempty, mempty, mempty
                          , defaultClientInstallFlags )
  }
  where
    get1 (a,_,_,_,_,_) = a; set1 a (_,b,c,d,e,f) = (a,b,c,d,e,f)
    get2 (_,b,_,_,_,_) = b; set2 b (a,_,c,d,e,f) = (a,b,c,d,e,f)
    get3 (_,_,c,_,_,_) = c; set3 c (a,b,_,d,e,f) = (a,b,c,d,e,f)
    get4 (_,_,_,d,_,_) = d; set4 d (a,b,c,_,e,f) = (a,b,c,d,e,f)
    get5 (_,_,_,_,e,_) = e; set5 e (a,b,c,d,_,f) = (a,b,c,d,e,f)
    get6 (_,_,_,_,_,f) = f; set6 f (a,b,c,d,e,_) = (a,b,c,d,e,f)


-- | The @install@ command actually serves four different needs. It installs:
-- * exes:
--   For example a program from hackage. The behavior is similar to the old
--   install command, except that now conflicts between separate runs of the
--   command are impossible thanks to the store.
--   Exes are installed in the store like a normal dependency, then they are
--   symlinked/copied in the directory specified by --installdir.
--   To do this we need a dummy projectBaseContext containing the targets as
--   estra packages and using a temporary dist directory.
-- * libraries
--   Libraries install through a similar process, but using GHC environment
--   files instead of symlinks. This means that 'v2-install'ing libraries
--   only works on GHC >= 8.0.
--
-- For more details on how this works, see the module
-- "Distribution.Client.ProjectOrchestration"
--
installAction
  :: ( ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags, TestFlags
     , ClientInstallFlags)
  -> [String] -> GlobalFlags
  -> IO ()
installAction (configFlags, configExFlags, installFlags, haddockFlags, testFlags
              , clientInstallFlags' )
              targetStrings globalFlags = do
  -- We never try to build tests/benchmarks for remote packages.
  -- So we set them as disabled by default and error if they are explicitly
  -- enabled.
  when (configTests configFlags' == Flag True) $
    die' verbosity $ "--enable-tests was specified, but tests can't "
                  ++ "be enabled in a remote package"
  when (configBenchmarks configFlags' == Flag True) $
    die' verbosity $ "--enable-benchmarks was specified, but benchmarks can't "
                  ++ "be enabled in a remote package"

  -- We cannot use establishDummyProjectBaseContext to get these flags, since
  -- it requires one of them as an argument. Normal establishProjectBaseContext
  -- does not, and this is why this is done only for the install command
  clientInstallFlags <- do
    let configFileFlag = globalConfigFile globalFlags
    savedConfig <- loadConfig verbosity configFileFlag
    pure $ savedClientInstallFlags savedConfig `mappend` clientInstallFlags'

  let 
    argParsec :: String -> Either String ParsedInstallTarget
    argParsec = either (Left . show) Right
              . runParsecParser parsec "command"
              . fieldLineStreamFromString

  rawInstallTargets <- case mapM argParsec targetStrings of
    Left err      -> die' verbosity err
    Right targets -> pure targets

  let
    installLibs    = fromFlagOrDefault False (cinstInstallLibs clientInstallFlags)
    targetFilter   = if installLibs then [LibKind] else [ExeKind]

    withProject = do
      ProjectBaseContext { distDirLayout, cabalDirLayout, projectConfig, localPackages }
        <- establishProjectBaseContext verbosity cliConfig InstallCommand
      let projectRoot = distProjectRootDirectory distDirLayout
      return (projectRoot, cabalDirLayout, projectConfig, localPackages)

    withoutProject globalConfig = do
      let 
        projectConfig = globalConfig <> cliConfig
        fakeProjectRoot = error "cabal panic! Project root is only used when there are local packages!"
      cabalDirLayout <- getCabalDirLayout projectConfig
      return (fakeProjectRoot, cabalDirLayout, projectConfig, [])

  (projectRoot, cabalDirLayout, config, localPackages) <-
    withProjectOrGlobalConfig verbosity globalConfigFlag withProject withoutProject

  home <- getHomeDirectory

  (compiler@Compiler { compilerId =
    compilerId@(CompilerId _ compilerVersion) }, platform, _) <-
      getCompiler verbosity config

  let
    globalEnv name =
      home </> ".ghc" </> ghcPlatformAndVersionString platform compilerVersion
           </> "environments" </> name
    localEnv dir =
      dir </>
      ".ghc.environment." <> ghcPlatformAndVersionString platform compilerVersion

  envFile <- case flagToMaybe (cinstEnvironmentPath clientInstallFlags) of
    Just spec
      -- Is spec a bare word without any "pathy" content? 
      -- Then it refers to a named global environment.
      | takeBaseName spec == spec -> return (globalEnv spec)
      | otherwise                 -> do
        spec' <- makeAbsolute spec
        isDir <- doesDirectoryExist spec'
        if isDir
          -- If spec is a directory, then make an ambient environment inside
          -- that directory.
          then return (localEnv spec')
          -- Otherwise, treat it like a literal file path.
          else return spec'
    Nothing                       -> return (globalEnv "default")

  print rawInstallTargets

  SourcePackageDb{ packageIndex } <- withRepoContext verbosity config (getSourcePackages verbosity)

  print (resolveInstallTarget targetFilter packageIndex localPackages <$> rawInstallTargets)
  where
    configFlags' = disableTestsBenchsByDefault configFlags
    verbosity = fromFlagOrDefault normal (configVerbosity configFlags')
    cliConfig = commandLineFlagsToProjectConfig
                  globalFlags configFlags' configExFlags
                  installFlags clientInstallFlags'
                  haddockFlags testFlags
    globalConfigFlag = projectConfigConfigFile (projectConfigShared cliConfig)

getCabalDirLayout :: ProjectConfig -> IO CabalDirLayout
getCabalDirLayout projectConfig = do
  cabalDir <- getCabalDir

  let 
    ProjectConfigBuildOnly {
      projectConfigLogsDir
    } = projectConfigBuildOnly projectConfig

    ProjectConfigShared {
      projectConfigStoreDir
    } = projectConfigShared projectConfig

    mstoreDir = flagToMaybe projectConfigStoreDir
    mlogsDir = flagToMaybe projectConfigLogsDir
    
  return (mkCabalDirLayout cabalDir mstoreDir mlogsDir)


withRepoContext :: Verbosity -> ProjectConfig -> (RepoContext -> IO a) -> IO a
withRepoContext verbosity projectConfig f = 
  projectConfigWithSolverRepoContext verbosity
    (projectConfigShared projectConfig)
    (projectConfigBuildOnly projectConfig)
    f

getCompiler :: Verbosity -> ProjectConfig -> IO (Compiler, Platform, ProgramDb)
getCompiler verbosity config = do
  let
    ProjectConfig {
      projectConfigShared = ProjectConfigShared {
        projectConfigHcFlavor,
        projectConfigHcPath,
        projectConfigHcPkg
      },
      projectConfigLocalPackages = PackageConfig {
        packageConfigProgramPaths,
        packageConfigProgramArgs,
        packageConfigProgramPathExtra
      }
    } = config

    hcFlavor = flagToMaybe projectConfigHcFlavor
    hcPath   = flagToMaybe projectConfigHcPath
    hcPkg    = flagToMaybe projectConfigHcPkg

    progDb =
        userSpecifyPaths (Map.toList (getMapLast packageConfigProgramPaths))
      . userSpecifyArgss (Map.toList (getMapMappend packageConfigProgramArgs))
      . modifyProgramSearchPath
          (++ [ ProgramSearchPathDir dir
              | dir <- fromNubList packageConfigProgramPathExtra ])
      $ defaultProgramDb
  
  configCompilerEx hcFlavor hcPath hcPkg progDb verbosity

isTargetLocal :: [Unresolved PackageSpecifier] -> InstallTarget a -> Maybe UnresolvedSourcePackage
isTargetLocal localPackages target = getFirst $ foldMap go localPackages
  where
    go (SpecificSourcePackage pkg)
      | packageId pkg == packageId target = First (Just pkg)
    go _                                  = First Nothing

data InstallTargetResolutionError = TargetPackageNotFound PackageId
                                  | TargetPackageInPrivateRepo PackageId
                                  | TargetPackageEmpty PackageId
                                  | TargetPackageHasNoComponent PackageId ParsedTarget
                                  | TargetComponentExcludedByFilter PackageId ComponentKind
                                  | TargetComponentAmbiguous PackageId UnqualComponentName
                                  deriving (Eq, Show) 

resolveInstallTarget :: [ComponentKind] -> Unresolved SPI.PackageIndex 
                     -> [Unresolved PackageSpecifier]
                     -> ParsedInstallTarget
                     -> Either InstallTargetResolutionError [ResolvedInstallTarget]
resolveInstallTarget kinds index localPackages target = do
  let 
    targetId = packageId target
    targetComp = itComp target
  (pkg, wrap) <- case isTargetLocal localPackages target of
    Just pkg -> return (pkg, Local)
    Nothing -> case SPI.lookupPackageId index targetId of
      Just pkg -> return (pkg, Repo)
      Nothing -> Left (TargetPackageNotFound targetId)

  let 
    pd = flattenPackageDescription $ packageDescription pkg

    unqualEq n c = maybe False (== n) (componentNameString c)

    libraries = filter ((== LibraryVisibilityPublic) . libVisibility) (allLibraries pd)
    libNames   = CLibName   . libName        <$> libraries
    exeNames   = CExeName   . exeName        <$> executables pd
    flibNames  = CFLibName  . foreignLibName <$> foreignLibs pd
    testNames  = CTestName  . testName       <$> testSuites pd
    benchNames = CBenchName . benchmarkName  <$> benchmarks pd
    allNames = concat [libNames, exeNames, flibNames, testNames, benchNames]

  names <- case targetComp of
    Just (Unqual n) -> case filter (unqualEq n) allNames of
      [c]   
        | componentKind c `elem` kinds -> return [c]
        | otherwise -> Left (TargetComponentExcludedByFilter targetId $ componentKind c)
      (_:_) -> Left (TargetComponentAmbiguous targetId n)
    Just (Qual c)
      | c `elem` allNames -> if componentKind c `elem` kinds
        then return [c]
        else Left (TargetComponentExcludedByFilter targetId $ componentKind c)
    Just (Filter f)
      | f `notElem` kinds -> Left (TargetComponentExcludedByFilter targetId f)
    Just (Filter LibKind)   -> return libNames
    Just (Filter ExeKind)   -> return exeNames
    Just (Filter FLibKind)  -> return flibNames
    Just (Filter TestKind)  -> return testNames
    Just (Filter BenchKind) -> return benchNames
    Just comp -> Left (TargetPackageHasNoComponent targetId comp)
    
    Nothing
      | let out = filter (flip elem kinds . componentKind) allNames
      , not (null out) -> return out
      | otherwise -> Left (TargetPackageEmpty targetId)

  return (InstallTarget targetId . wrap <$> names)
  
-- | Install any built exe by symlinking/copying it
-- we don't use BuildOutcomes because we also need the component names
installExes
  :: Verbosity
  -> ProjectBaseContext
  -> ProjectBuildContext
  -> Platform
  -> Compiler
  -> ClientInstallFlags
  -> IO ()
installExes verbosity baseCtx buildCtx platform compiler
            clientInstallFlags = do
  let storeDirLayout = cabalStoreDirLayout $ cabalDirLayout baseCtx

      mkUnitBinDir :: UnitId -> FilePath
      mkUnitBinDir =
        InstallDirs.bindir .
        storePackageInstallDirs' storeDirLayout (compilerId compiler)

      mkExeName :: UnqualComponentName -> FilePath
      mkExeName exe = unUnqualComponentName exe <.> exeExtension platform
      installdirUnknown =
        "installdir is not defined. Set it in your cabal config file "
        ++ "or use --installdir=<path>"

  installdir <- fromFlagOrDefault (die' verbosity installdirUnknown) $
                pure <$> cinstInstalldir clientInstallFlags
  createDirectoryIfMissingVerbose verbosity False installdir
  warnIfNoExes verbosity buildCtx
  let
    doInstall = installUnitExes
                  verbosity
                  overwritePolicy
                  mkUnitBinDir mkExeName
                  installdir installMethod
    in traverse_ doInstall $ Map.toList $ targetsMap buildCtx
  where
    overwritePolicy = fromFlagOrDefault NeverOverwrite $
                      cinstOverwritePolicy clientInstallFlags
    installMethod   = fromFlagOrDefault InstallMethodSymlink $
                      cinstInstallMethod clientInstallFlags

-- | Install any built library by adding it to the default ghc environment
installLibraries
  :: Verbosity
  -> Compiler
  -> PackageDBStack
  -> FilePath -- ^ Environment file
  -> [GhcEnvironmentFileEntry]
  -> IO ()
installLibraries verbosity compiler packageDbs envFile envEntries = do
  -- Why do we get it again? If we updated a globalPackage then we need
  -- the new version.
  if supportsPkgEnvFiles $ getImplInfo compiler
    then do
      let
        baseEntries =
          GhcEnvFileClearPackageDbStack : fmap GhcEnvFilePackageDb packageDbs
        contents' = renderGhcEnvironmentFile (baseEntries ++ envEntries)
      createDirectoryIfMissing True (takeDirectory envFile)
      writeFileAtomic envFile (BS.pack contents')
    else
      warn verbosity $
          "The current compiler doesn't support safely installing libraries, "
        ++ "so only executables will be available. (Library installation is "
        ++ "supported on GHC 8.0+ only)"

warnIfNoExes :: Verbosity -> ProjectBuildContext -> IO ()
warnIfNoExes verbosity buildCtx =
  when noExes $
    warn verbosity $
    "You asked to install executables, but there are no executables in "
    <> plural (listPlural selectors) "target" "targets" <> ": "
    <> intercalate ", " (showTargetSelector <$> selectors) <> ". "
    <> "Perhaps you want to use --lib to install libraries instead."
  where
    targets    = concat $ Map.elems $ targetsMap buildCtx
    components = fst <$> targets
    selectors  = concatMap snd targets
    noExes     = null $ catMaybes $ exeMaybe <$> components

    exeMaybe (ComponentTarget (CExeName exe) _) = Just exe
    exeMaybe _                                  = Nothing

initialGlobalPackage :: PI.InstalledPackageIndex -> GenericPackageDescription
initialGlobalPackage ipi = genericPackageDescription
  where
    globals = PI.allPackagesByName ipi

    pkgInfoToDep :: InstalledPackageInfo -> Dependency
    pkgInfoToDep InstalledPackageInfo
      { sourcePackageId = PackageIdentifier{..}, sourceLibName } =
        Dependency pkgName (thisVersion pkgVersion) (Set.singleton sourceLibName)

    genericPackageDescription = emptyGenericPackageDescription 
      & L.packageDescription .~ packageDescription
      & L.condLibrary        .~ Just (CondNode library globalDeps [])
    packageDescription = emptyPackageDescription
      { package = fakePackageId
      , specVersionRaw = Left (mkVersion [2, 2])
      , licenseRaw = Left SPDX.NONE
      }
    library = emptyLibrary { libBuildInfo = buildInfo }
    buildInfo = emptyBuildInfo
      { targetBuildDepends = globalDeps
      , defaultLanguage = Just Haskell2010
      }
    globalDeps = fmap (pkgInfoToDep . head . snd) globals

addDependencies :: GenericPackageDescription 
                -> [Dependency] -> [ExeDependency]
                -> GenericPackageDescription
addDependencies pd libs exes = pd''
  where
    pd'  = pd  & (\f -> L.allCondTrees     $ traverseCondTreeC f)    %~ (libs ++)
    pd'' = pd' & (\f -> L.traverseBuildInfos $ L.buildToolDepends f) %~ (exes ++)

-- | Disables tests and benchmarks if they weren't explicitly enabled.
disableTestsBenchsByDefault :: ConfigFlags -> ConfigFlags
disableTestsBenchsByDefault configFlags =
  configFlags { configTests = Flag False <> configTests configFlags
              , configBenchmarks = Flag False <> configBenchmarks configFlags }

-- | Symlink/copy every exe from a package from the store to a given location
installUnitExes
  :: Verbosity
  -> OverwritePolicy -- ^ Whether to overwrite existing files
  -> (UnitId -> FilePath) -- ^ A function to get an UnitId's
                          -- ^ store directory
  -> (UnqualComponentName -> FilePath) -- ^ A function to get an
                                       -- ^ exe's filename
  -> FilePath
  -> InstallMethod
  -> ( UnitId
     , [(ComponentTarget, [TargetSelector])] )
  -> IO ()
installUnitExes verbosity overwritePolicy
                mkSourceBinDir mkExeName
                installdir installMethod
                (unit, components) =
  traverse_ installAndWarn exes
  where
    exes = catMaybes $ (exeMaybe . fst) <$> components
    exeMaybe (ComponentTarget (CExeName exe) _) = Just exe
    exeMaybe _ = Nothing
    installAndWarn exe = do
      success <- installBuiltExe
                   verbosity overwritePolicy
                   (mkSourceBinDir unit) (mkExeName exe)
                   installdir installMethod
      let errorMessage = case overwritePolicy of
            NeverOverwrite ->
              "Path '" <> (installdir </> prettyShow exe) <> "' already exists. "
              <> "Use --overwrite-policy=always to overwrite."
            -- This shouldn't even be possible, but we keep it in case
            -- symlinking/copying logic changes
            AlwaysOverwrite ->
              case installMethod of
                InstallMethodSymlink -> "Symlinking"
                InstallMethodCopy    ->
                  "Copying" <> " '" <> prettyShow exe <> "' failed."
      unless success $ die' verbosity errorMessage

-- | Install a specific exe.
installBuiltExe
  :: Verbosity -> OverwritePolicy
  -> FilePath -- ^ The directory where the built exe is located
  -> FilePath -- ^ The exe's filename
  -> FilePath -- ^ the directory where it should be installed
  -> InstallMethod
  -> IO Bool -- ^ Whether the installation was successful
installBuiltExe verbosity overwritePolicy
                sourceDir exeName
                installdir InstallMethodSymlink = do
  notice verbosity $ "Symlinking '" <> exeName <> "'"
  symlinkBinary
    overwritePolicy
    installdir
    sourceDir
    (mkUnqualComponentName exeName)
    exeName
installBuiltExe verbosity overwritePolicy
                sourceDir exeName
                installdir InstallMethodCopy = do
  notice verbosity $ "Copying '" <> exeName <> "'"
  exists <- doesPathExist destination
  case (exists, overwritePolicy) of
    (True , NeverOverwrite ) -> pure False
    (True , AlwaysOverwrite) -> remove >> copy
    (False, _              ) -> copy
  where
    source = sourceDir </> exeName
    destination = installdir </> exeName
    remove = do
      isDir <- doesDirectoryExist destination
      if isDir
      then removeDirectory destination
      else removeFile      destination
    copy = copyFile source destination >> pure True

-- | Create a dummy project context, without a .cabal or a .cabal.project file
-- (a place where to put a temporary dist directory is still needed)
establishDummyProjectBaseContext
  :: Verbosity
  -> ProjectConfig
  -> FilePath
     -- ^ Where to put the dist directory
  -> [PackageSpecifier UnresolvedSourcePackage]
     -- ^ The packages to be included in the project
  -> CurrentCommand
  -> IO ProjectBaseContext
establishDummyProjectBaseContext verbosity cliConfig tmpDir
                                 localPackages currentCommand = do
    cabalDir <- getCabalDir

    -- Create the dist directories
    createDirectoryIfMissingVerbose verbosity True $ distDirectory distDirLayout
    createDirectoryIfMissingVerbose verbosity True $
      distProjectCacheDirectory distDirLayout

    globalConfig <- runRebuild ""
                  $ readGlobalConfig verbosity
                  $ projectConfigConfigFile
                  $ projectConfigShared cliConfig
    let projectConfig = globalConfig <> cliConfig

    let ProjectConfigBuildOnly {
          projectConfigLogsDir
        } = projectConfigBuildOnly projectConfig

        ProjectConfigShared {
          projectConfigStoreDir
        } = projectConfigShared projectConfig

        mlogsDir = flagToMaybe projectConfigLogsDir
        mstoreDir = flagToMaybe projectConfigStoreDir
        cabalDirLayout = mkCabalDirLayout cabalDir mstoreDir mlogsDir

        buildSettings = resolveBuildTimeSettings
                          verbosity cabalDirLayout
                          projectConfig

    return ProjectBaseContext {
      distDirLayout,
      cabalDirLayout,
      projectConfig,
      localPackages,
      buildSettings,
      currentCommand
    }
  where
    mdistDirectory = flagToMaybe
                   $ projectConfigDistDir
                   $ projectConfigShared cliConfig
    projectRoot = ProjectRootImplicit tmpDir
    distDirLayout = defaultDistDirLayout projectRoot
                                         mdistDirectory

-- | This defines what a 'TargetSelector' means for the @bench@ command.
-- It selects the 'AvailableTarget's that the 'TargetSelector' refers to,
-- or otherwise classifies the problem.
--
-- For the @build@ command select all components except non-buildable
-- and disabled tests\/benchmarks, fail if there are no such
-- components
--
selectPackageTargets
  :: TargetSelector
  -> [AvailableTarget k] -> Either TargetProblem [k]
selectPackageTargets targetSelector targets

    -- If there are any buildable targets then we select those
  | not (null targetsBuildable)
  = Right targetsBuildable

    -- If there are targets but none are buildable then we report those
  | not (null targets)
  = Left (TargetProblemNoneEnabled targetSelector targets')

    -- If there are no targets at all then we report that
  | otherwise
  = Left (TargetProblemNoTargets targetSelector)
  where
    targets'         = forgetTargetsDetail targets
    targetsBuildable = selectBuildableTargetsWith
                         (buildable targetSelector)
                         targets

    -- When there's a target filter like "pkg:tests" then we do select tests,
    -- but if it's just a target like "pkg" then we don't build tests unless
    -- they are requested by default (i.e. by using --enable-tests)
    buildable (TargetPackage _ _  Nothing) TargetNotRequestedByDefault = False
    buildable (TargetAllPackages  Nothing) TargetNotRequestedByDefault = False
    buildable _ _ = True

-- | For a 'TargetComponent' 'TargetSelector', check if the component can be
-- selected.
--
-- For the @build@ command we just need the basic checks on being buildable etc.
--
selectComponentTarget
  :: SubComponentTarget
  -> AvailableTarget k -> Either TargetProblem k
selectComponentTarget subtarget =
    either (Left . TargetProblemCommon) Right
  . selectComponentTargetBasic subtarget


-- | The various error conditions that can occur when matching a
-- 'TargetSelector' against 'AvailableTarget's for the @build@ command.
--
data TargetProblem =
     TargetProblemCommon       TargetProblemCommon

     -- | The 'TargetSelector' matches targets but none are buildable
   | TargetProblemNoneEnabled TargetSelector [AvailableTarget ()]

     -- | There are no targets at all
   | TargetProblemNoTargets   TargetSelector
  deriving (Eq, Show)

reportTargetProblems :: Verbosity -> [TargetProblem] -> IO a
reportTargetProblems verbosity =
    die' verbosity . unlines . map renderTargetProblem

renderTargetProblem :: TargetProblem -> String
renderTargetProblem (TargetProblemCommon problem) =
    renderTargetProblemCommon "build" problem
renderTargetProblem (TargetProblemNoneEnabled targetSelector targets) =
    renderTargetProblemNoneEnabled "build" targetSelector targets
renderTargetProblem(TargetProblemNoTargets targetSelector) =
    renderTargetProblemNoTargets "build" targetSelector

reportCannotPruneDependencies :: Verbosity -> CannotPruneDependencies -> IO a
reportCannotPruneDependencies verbosity =
    die' verbosity . renderCannotPruneDependencies
