{-# LANGUAGE CPP #-}

module Main (main) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (pure, (<$>))
#endif

import Control.Exception (bracket_, finally, onException)
import Control.Monad.Extra
import Data.Char
import Data.List.Extra
import Data.Maybe
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,11,0))
#else
import Data.Monoid ((<>))
#endif
import SimpleCabal
import SimpleCmd
#if MIN_VERSION_simple_cmd(0,2,1)
  hiding (whenM)
#endif
import SimpleCmd.Git
import SimpleCmdArgs
import System.Directory
import System.Environment.XDG.BaseDir
import System.FilePath
import System.IO (BufferMode(NoBuffering), hSetBuffering, hSetEcho,
                  stdin, stdout)
import Paths_hkgr (getDataFileName, version)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  simpleCmdArgs (Just version) "Hackage Release tool"
    "'Hackager' is a package release tool for easy Hackage workflow" $
    subcommands
    [ Subcommand "new" "setup a new project" $
      newCmd <$> optional (strArg "PROJECT")
    , Subcommand "tagdist" "'git tag' version and 'cabal sdist' tarball" $
      tagDistCmd <$> forceOpt "Move existing tag"
    , Subcommand "upload" "'cabal upload' candidate tarball to Hackage" $
      pure $ uploadCmd False
    , Subcommand "publish" "Publish to Hackage ('cabal upload --publish')" $
      pure $ uploadCmd True
    , Subcommand "upload-haddock" "Upload candidate documentation to Hackage" $
      pure $ upHaddockCmd False
    , Subcommand "publish-haddock" "Publish documentation to Hackage" $
      pure $ upHaddockCmd True
    , Subcommand "version" "Show the package version from .cabal file" $
      pure showVersionCmd
    ]
  where
    forceOpt = switchWith 'f' "force"

tagDistCmd :: Bool -> IO ()
tagDistCmd force = do
  needProgram "cabal"
  diff <- git "diff" ["HEAD"]
  unless (null diff) $ do
    putStrLn "=== start of uncommitted changes ==="
    putStrLn diff
    putStrLn "=== end of uncommitted changes ==="
  pkgid <- checkPackage
  let tag = pkgidTag pkgid
  tagHash <- cmdMaybe "git" ["rev-parse", tag]
  when (isJust tagHash && not force) $
    error' $ "tag " ++ tag ++ " exists: use --force to override and move"
  git_ "tag" $ ["--force" | force] ++ [tag]
  unless force $ putStrLn tag
  sdist force pkgid `onException` do
    putStrLn "Resetting tag"
    if force
      then git_ "tag" ["--force", tag, fromJust tagHash]
      else git_ "tag" ["--delete", tag]

pkgidTag :: PackageIdentifier -> String
pkgidTag pkgid = "v" ++ packageVersion pkgid

checkPackage :: IO PackageIdentifier
checkPackage = do
  pkgid <- getPackageId
  checkVersionCommitted pkgid
  checkNotPublished pkgid
  return pkgid

checkVersionCommitted :: PackageIdentifier -> IO ()
checkVersionCommitted pkgid = do
  let pkg = packageName pkgid
  diff <- git "diff" ["-U0", "HEAD", unPackageName pkg <.> "cabal"]
  when ("version:" `isInfixOf` map toLower diff) $
    error' "Please commit or revert the package Version first"

checkNotPublished :: PackageIdentifier -> IO ()
checkNotPublished pkgid = do
  let published = sdistDir </> showPkgId pkgid <.> ".tar.gz" <.> "published"
  exists <- doesFileExist published
  when exists $ error' $ showPkgId pkgid <> " was already published!!"
  let oldpublished = "dist" </> showPkgId pkgid <.> ".tar.gz" <.> "published"
  oldExists <- doesFileExist oldpublished
  when oldExists $ error' $ showPkgId pkgid <> " was already published!!"

sdistDir :: FilePath
sdistDir = "dist-newstyle/sdist"

sdist :: Bool -> PackageIdentifier -> IO ()
sdist force pkgid = do
  let tag = pkgidTag pkgid
  let target = sdistDir </> showPkgId pkgid <.> ".tar.gz"
  haveTarget <- doesFileExist target
  when haveTarget $
    if force
    then removeFile target
    else error' $ target <> " exists already!"
  cwd <- getCurrentDirectory
  withTempDirectory "tmp-sdist" $ do
    git_ "clone" ["-q", "--no-checkout", "..", "."]
    git_ "checkout" ["-q", tag]
    cabal_ "check" []
    mhlint <- findExecutable "hlint"
    when (isJust mhlint) $ do
      putStrLn "Running hlint"
      void $ cmdBool "hlint" ["--no-summary", "."]
    let dest = takeDirectory $ cwd </> target
    unlessM (doesDirectoryExist dest) $
      createDirectoryIfMissing True dest
    cabal_ "v2-sdist" ["--output-dir=" ++ dest]

showVersionCmd :: IO ()
showVersionCmd = do
  pkgid <- getPackageId
  putStrLn $ packageVersion pkgid

uploadCmd :: Bool -> IO ()
uploadCmd publish = do
  pkgid <- checkPackage
  let file = sdistDir </> showPkgId pkgid <.> ".tar.gz"
  exists <- doesFileExist file
  unless exists $ tagDistCmd False
  when publish $ do
    let tag = pkgidTag pkgid
    tagHash <- cmd "git" ["rev-parse", tag]
    branch <- cmd "git" ["branch", "--show-current"]
    git_ "push" ["origin", tagHash ++ ":" ++ branch]
    git_ "push" ["origin", tag]
  username <- prompt False "Hackage username"
  passwd <- prompt True "Hackage password"
  void $ cmdStdIn "cabal" ("upload" : ["--publish" | publish] ++ [file]) $
    unlines [username, passwd]
  putStrLn $ (if publish then "Published at " else "Uploaded to ") ++ "https://hackage.haskell.org/package/" ++ showPkgId pkgid ++ if publish then "" else "/candidate"
  when publish $
    createFileLink (takeFileName file) (file <.> "published")
  where
    prompt :: Bool -> String -> IO String
    prompt hide s = do
      putStr $ s ++ ": "
      inp <- if hide then withoutEcho getLine else getLine
      when hide $ putChar '\n'
      return inp
      where
        withoutEcho :: IO a -> IO a
        withoutEcho action =
          finally (hSetEcho stdin False >> action) (hSetEcho stdin True)

upHaddockCmd :: Bool -> IO ()
upHaddockCmd publish =
  cabal_ "upload" $ "--documentation" : ["--publish" | publish]

cabal_ :: String -> [String] -> IO ()
cabal_ c args =
  cmd_ "cabal" (c:args)

withTempDirectory :: FilePath -> IO a -> IO a
withTempDirectory dir run =
  bracket_ (createDirectory dir) (removeDirectoryRecursive dir) $
  withCurrentDirectory dir run

#if (defined(MIN_VERSION_simple_cmd) && MIN_VERSION_simple_cmd(0,2,1))
#else
needProgram :: String -> IO ()
needProgram prog = do
  mx <- findExecutable prog
  unless (isJust mx) $ error' $ "program needs " ++ prog
#endif

-- FIXME add default templates dir
-- FIXME --license
newCmd :: Maybe String -> IO ()
newCmd mproject = do
  name <- case mproject of
    Just ns -> return ns
    Nothing -> do
      files <- listDirectory "."
      if null files then takeFileName <$> getCurrentDirectory
        else do
        -- filter out dirs
        dirs <- filterM doesDirectoryExist files
        if dirs == files then error' "Could not guess name"
          else
          case filter ("cabal" `isExtensionOf`) $ files \\ dirs of
            [] -> takeFileName <$> getCurrentDirectory
            [cbl] -> return $ takeBaseName cbl
            _ -> error' "More than one .cabal file found!"
  when (isJust mproject) $ createDirectory name >> setCurrentDirectory name
  mcabal <- checkForCabalFile
  case mcabal of
    Nothing -> do
      let setupFile = "Setup.hs"
      origsetup <- doesFileExist setupFile
      cabal_ "init" ["--quiet", "--no-comments", "--non-interactive", "--is-libandexe", "--cabal-version=1.18", "--license=BSD3", "--package-name=" ++ name, "--version=0.1.0", "--dependency=base<5", "--source-dir=app"]
      sed ["/module Main where/,+1 d"] "app/Main.hs"
      whenM (doesFileExist "CHANGELOG.md") $
        renameFile "CHANGELOG.md" "ChangeLog.md"
      unlessM (doesFileExist "README.md") $
        writeFile "README.md" $ "# " ++ name ++ "\n"
      unless origsetup $ do
        setup <- doesFileExist setupFile
        when setup $ removeFile setupFile
      setupCabalTemplate name
    Just cblName ->
      when (cblName /= name) $
      putStrLn $ "Warning: " ++ cblName ++ " different to " ++ name
  haveStackCfg <- doesFileExist "stack.yaml"
  mstack <- findExecutable "stack"
  -- FIXME add stack.yaml template too
  when (not haveStackCfg && isJust mstack) $ do
    cmd_ "stack" ["init", "--verbosity", "warn", "--resolver", "lts-14"]
    sed ["/^#/d", "/^$/d"] "stack.yaml"
  haveGit <- doesDirectoryExist ".git"
  unless haveGit $ do
    git_ "init" ["-q"]
    git_ "add" [name <.> "cabal"]
  where
    checkForCabalFile :: IO (Maybe String)
    checkForCabalFile = do
      files <- listDirectory "."
      case filter ("cabal" `isExtensionOf`) files of
        [] -> return Nothing
        [cbl] -> return $ Just (takeBaseName cbl)
        _ -> error' "More than one .cabal file found!"

    sed :: [String] -> FilePath -> IO ()
    sed args file =
      cmd_ "sed" $ ["-i", "-e"] ++ intersperse "-e" args ++ [file]

    setupCabalTemplate :: String -> IO ()
    setupCabalTemplate name = do
      userTemplate <- getUserConfigFile "hkgr" "template.cabal"
      unlessM (doesFileExist userTemplate) $ do
        origTemplate <- getDataFileName "template.cabal.tmpl"
        createDirectoryIfMissing True $ takeDirectory userTemplate
        -- FIXME put a copy of the current template there too for reference
        copyFile origTemplate userTemplate
        username <- git "config" ["--global", "user.name"]
        replaceHolder "NAME" username userTemplate
        usermail <-
          fromMaybeM (git "config" ["--global", "user.email"]) $
            cmdMaybe "git" ["config", "--global", "github.email"]
        replaceHolder "EMAIL" usermail userTemplate
        githubuser <- git "config" ["--global", "github.user"]
        replaceHolder "USER" githubuser userTemplate
        putStrLn $ userTemplate ++ " set up"
      copyFile userTemplate $ name <.> "cabal"
      replaceHolder "PROJECT" name $ name <.> "cabal"
      replaceHolder "PROJECT_" (map underscore name) $ name <.> "cabal"
      replaceHolder "SUMMARY" (name ++ " project") $ name <.> "cabal"
      year <- cmd "date" ["+%Y"]
      replaceHolder "YEAR" year $ name <.> "cabal"
      let moduleName = toModule name
      replaceHolder "MODULE" moduleName $ name <.> "cabal"
      let modulePath = "src" </> intercalate "/" (wordsBy (== '.') moduleName) <.> "hs"
      createDirectoryIfMissing True $ takeDirectory modulePath
      writeFile modulePath $ "module " ++ moduleName ++ " where \n"
      where
        replaceHolder lbl val file =
          sed ["s/@" ++ lbl ++ "@/" ++ val ++ "/"] file

        underscore '-' = '_'
        underscore c = c

        toModule pkg =
          let titlecase part =
                if null part then part
                else toUpper (head part) : tail part
          in intercalate "." $ map titlecase $ wordsBy (== '-') pkg

#if !MIN_VERSION_filepath(1,4,2)
    isExtensionOf :: String -> FilePath -> Bool
    isExtensionOf ext@('.':_) = isSuffixOf ext . takeExtensions
    isExtensionOf ext         = isSuffixOf ('.':ext) . takeExtensions
#endif

#if !MIN_VERSION_extra(1,6,15)
fromMaybeM :: Monad m => m a -> m (Maybe a) -> m a
fromMaybeM n = maybeM n pure
#endif
