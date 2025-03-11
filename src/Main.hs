{-# LANGUAGE CPP #-}

module Main (main) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (pure, (<$>))
#endif

import Control.Exception (bracket_, onException)
import Control.Monad.Extra
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char
import Data.List.Extra
import Data.Maybe
import Data.Version.Extra
import SimpleCabal
import SimpleCmd ((+-+))
import SimpleCmdArgs
import SimplePrompt (promptNonEmpty, promptPassword)
import System.Directory
import System.Environment.XDG.BaseDir
import System.Exit (ExitCode (..))
import System.FilePath
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)
import qualified System.Process.Typed as P
import Paths_hkgr (getDataFileName, version)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  simpleCmdArgs (Just version) "Hackage Release tool"
    "'Hackager' is a package release tool for easy Hackage workflow" $
    subcommands
    [ Subcommand "new" "setup a new project" $
      newCmd
      <$> optional (strOptionWith 'l' "license" "LICENSE" "Specify license")
      <*> optional (strArg "PROJECT")
    , Subcommand "tagdist" "'git tag' version and 'cabal sdist' tarball" $
      tagDistCmd
      <$> existingTag
      <*> forceOpt "Move existing tag"
      <*> hlintOpt
      <*> nobuildOpt
    , Subcommand "upload" "'cabal upload' candidate tarball to Hackage" $
      uploadCmd False
      <$> existingTag
      <*> forceOpt "Move existing tag"
      <*> hlintOpt
      <*> nobuildOpt
      <*> pure False
    , Subcommand "publish" "git push and cabal upload --publish" $
      uploadCmd True False False True
      <$> nobuildOpt
      <*> switchWith 'U' "no-upload" "Do not upload to Hackage"
    , Subcommand "upload-haddock" "Upload candidate documentation to Hackage" $
      upHaddockCmd False
      <$> forceOpt "rebuild doc tarball"
    , Subcommand "publish-haddock" "Publish documentation to Hackage" $
      upHaddockCmd True
      <$> forceOpt "rebuild doc tarball"
    , Subcommand "build" "Do a local pristine build from the tarball" $
      pure pristineBuildCmd
    , Subcommand "version" "Show the package version from .cabal file" $
      pure showVersionCmd
    , Subcommand "rename" "Rename the Cabal package" $
      renameCmd
      <$> strArg "NEWNAME"
    , Subcommand "github" "Add github repo" $
      pure githubCmd
    ]
  where
    forceOpt = switchWith 'f' "force"

    existingTag =
      switchWith 'T' "existing-tag" "Use existing tag to create tarball"

    hlintOpt =
      switchWith 'H' "no-hlint" "Skip running hlint"

    nobuildOpt =
      switchWith 'N' "no-build" "Do not perform local sanity build"

git :: String -> [String] -> P.ProcessConfig () () ()
git c args = P.proc "git" (c:args)

git_ :: String -> [String] -> IO ()
git_ c args =
  cmd_ "git" (c:args)

gitBool :: String -> [String] -> IO Bool
gitBool c args = do
  (== ExitSuccess) <$> P.runProcess (git c args)

removeTrailingNewline :: B.ByteString -> B.ByteString
removeTrailingNewline "" = ""
removeTrailingNewline bs =
  if B.last bs == '\n'
  then B.init bs
  else bs

cmd :: String -> [String] -> IO String
cmd c args =
  B.unpack . removeTrailingNewline <$> P.readProcessStdout_ (P.proc c args)

cmd_ :: String -> [String] -> IO ()
cmd_ c args = P.runProcess_ $ P.proc c args

cmdOut :: P.ProcessConfig () () () -> IO String
cmdOut p =
  B.unpack . removeTrailingNewline <$> P.readProcessStdout_ p

cmdMaybe :: P.ProcessConfig () () () -> IO (Maybe String)
cmdMaybe p = do
  (ret, out ,_err) <- P.readProcess p
  return $ if ret == ExitSuccess
           then Just (B.unpack (removeTrailingNewline out))
           else Nothing

cmdLines :: P.ProcessConfig () () () -> IO [String]
cmdLines p =
  map B.unpack . B.lines <$> P.readProcessStdout_ p

-- from simple-cmd
error' :: String -> a
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,9,0))
error' = errorWithoutStackTrace
#else
error' = error
#endif

tarGzExt :: String
tarGzExt = "tar.gz"

assertTagOnBranch :: String -> IO ()
assertTagOnBranch tag =
    whenM (null <$> cmdOut (git "branch" ["--contains", "tags/" ++ tag])) $
    error' $ tag +-+ "is no longer on branch: use --force to move it"

tagDistCmd :: Bool -> Bool -> Bool -> Bool -> IO ()
tagDistCmd existingtag force noHlint nobuild = do
  pkgid <- checkPackage True
  let tag = pkgidTag pkgid
  mtagHash <- cmdMaybe (git "rev-parse" [tag])
  let tagexists = isJust mtagHash
  when (not force && tagexists) $ assertTagOnBranch tag
  if existingtag
    then if not tagexists
         then error' $ "tag" +-+ tag +-+ "does not exist"
         else sdist force noHlint nobuild pkgid
    else do
    if tagexists && not force
      then do
      headHash <- cmdOut (git "rev-parse" ["HEAD"])
      let onHead = Just headHash == mtagHash
      putStrLn $ "tag is" +-+ (if onHead then "" else "not") +-+ "on HEAD"
      let tarball = sdistDir </> showPkgId pkgid <.> tarGzExt
      exists <- doesFileExist tarball
      if exists
        then error' $ "tag" +-+ tag +-+ "exists: use --force to" +-+
             (if exists then "override tarball and" else "") +-+ "move"
        else sdist force noHlint nobuild pkgid
      else do
      git_ "tag" $ ["--force" | force] ++ [tag]
      unless force $ putStrLn tag
      -- actually always forced because: `== not (tagexists && not force)`
      sdist (force || not tagexists) noHlint nobuild pkgid
        `onException` do
        putStrLn "Resetting tag"
        if force
          then git_ "tag" ["--force", tag, fromJust mtagHash]
          else git_ "tag" ["--delete", tag]

pkgidTag :: PackageIdentifier -> String
pkgidTag pkgid = "v" ++ packageVersion pkgid

checkPackage :: Bool -> IO PackageIdentifier
checkPackage checkDiff = do
  when checkDiff $ do
    diff <- cmdOut $ git "diff" ["HEAD"]
    unless (null diff) $ do
      putStrLn "=== start of uncommitted changes ==="
      putStrLn diff
      putStrLn "=== end of uncommitted changes ==="
  pkgid <- getPackageId
  checkNameVersionCommitted pkgid
  checkNotPublished pkgid
  return pkgid
  where
    checkNameVersionCommitted :: PackageIdentifier -> IO ()
    checkNameVersionCommitted pkgid = do
      let pkg = packageName pkgid
          cabalfile = unPackageName pkg <.> "cabal"
      unlessM (doesFileExist cabalfile) $
        error' $ ".cabal filename differs from package name:" +-+ unPackageName pkg
      diff <- cmdOut (git "diff" ["-U0", "HEAD", cabalfile])
      when (any ("+version:" `isPrefixOf`) (lines (map toLower diff))) $
        error' "Please commit or revert the changed package Version first"

    checkNotPublished :: PackageIdentifier -> IO ()
    checkNotPublished pkgid = do
      let published = sdistDir </> showPkgId pkgid <.> tarGzExt <.> "published"
      exists <- doesFileExist published
      when exists $ error' $ showPkgId pkgid +-+ "was already published!!"
      let oldpublished = "dist" </> showPkgId pkgid <.> tarGzExt <.> "published"
      oldExists <- doesFileExist oldpublished
      when oldExists $ error' $ showPkgId pkgid +-+ "was already published!!"

sdistDir :: FilePath
sdistDir = ".hkgr"

sdist :: Bool -> Bool -> Bool -> PackageIdentifier -> IO ()
sdist force noHlint nobuild pkgid = do
  let tag = pkgidTag pkgid
  let target = sdistDir </> showPkgId pkgid <.> tarGzExt
  haveTarget <- doesFileExist target
  when haveTarget $
    if force
    then removeFile target
    else error' $ target +-+ "exists already!"
  cwd <- getCurrentDirectory
  withTempDirectory "tmp-sdist" $ do
    git_ "clone" ["-q", "--no-checkout", "..", "."]
    git_ "checkout" ["-q", tag]
    whenM (doesFileExist ".gitmodules") $
      git_ "submodule" ["update", "--init"]
    cabal_ "check" []
    unless noHlint $ do
      mhlint <- findExecutable "hlint"
      when (isJust mhlint) $ do
        putStrLn "Running hlint"
        void $ P.runProcess $ P.proc "hlint" ["--no-summary", "."]
    let dest = takeDirectory $ cwd </> target
    unlessM (doesDirectoryExist dest) $
      createDirectoryIfMissing True dest
    addCabalProject
    cabal_ "v2-sdist" ["--output-dir=" ++ dest]
  unless nobuild pristineBuildCmd

showVersionCmd :: IO ()
showVersionCmd = do
  pkgid <- getPackageId
  putStrLn $ packageVersion pkgid

gitListTag :: String -> IO Bool
gitListTag tag = do
  res <- cmdOut $ git "tag" ["--list", tag]
  return $ res == tag

-- FIXME cabal install creates tarballs now
uploadCmd :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> IO ()
uploadCmd publish existingtag force noHlint nobuild noupload = do
  needProgram "cabal"
  pkgid <- checkPackage False
  let tarball = sdistDir </> showPkgId pkgid <.> tarGzExt
      tag = pkgidTag pkgid
  tagexists <- gitListTag tag
  when (not tagexists && existingtag) $
    error' $ tag +-+ "does not exist"
  when (force || not tagexists) $
    tagDistCmd existingtag force noHlint nobuild
  assertTagOnBranch tag
  untagged <- cmdLines $ git "log" ["--pretty=reference", tag ++ "..HEAD"]
  unless (null untagged) $ do
    putStrLn $ "untagged newer commit" ++
      (if length untagged > 1 then "s" else "") ++ ":"
    mapM_ putStrLn untagged
  when publish $ do
    tagHash <- cmdOut $ git "rev-parse" [tag]
    branch <- cmdOut $ git "branch" ["--show-current"]
    mergeable <- gitBool "merge-base" ["--is-ancestor", "HEAD", tagHash]
    when mergeable $ do
      putStr "git pushing... "
      git_ "push" ["--quiet", "origin", tagHash ++ ":" ++ branch]
      putStrLn "done"
    git_ "push" ["origin", tag]
  unless noupload $ do
    userpassBS <- getUserPassword
    -- FIXME if password fails, repeat login
    void $ P.readProcessInterleaved_ (P.setStdin (P.byteStringInput userpassBS) $ P.proc "cabal" ("upload" : ["--publish" | publish] ++ [tarball]))
    putStrLn $ (if publish then "Published at" else "Uploaded to") +-+ "https://hackage.haskell.org/package/" ++ showPkgId pkgid ++ if publish then "" else "/candidate"
  when publish $
    createFileLink (takeFileName tarball) (tarball <.> "published")

getUserPassword :: IO B.ByteString
getUserPassword = do
  cabalConfig <- do
    home <- getHomeDirectory
    let cabalConfig = home </> ".cabal/config"
    exists <- doesFileExist cabalConfig
    if exists
      then readFile cabalConfig
      else do
      putStrLn $ "Warning:" +-+ cabalConfig +-+ "not found!"
      return ""
  muser <- maybeGetHackage "username" False cabalConfig
  mpasswd <- maybeGetHackage "password" True cabalConfig
  return $ B.pack $ unlines $ catMaybes [muser, mpasswd]
  where
    maybeGetHackage :: String -> Bool -> String -> IO (Maybe String)
    maybeGetHackage field hide cabalConfig =
      if haveCabalConfigField
        then return Nothing
        else Just <$> prompt hide ("hackage.haskell.org" +-+ field)
      where
        haveCabalConfigField :: Bool
        haveCabalConfigField =
          any ((field ++ ":") `isPrefixOf`) (lines cabalConfig)

prompt :: Bool -> String -> IO String
prompt hide s = do
  inp <- (if hide then promptPassword else promptNonEmpty) s
  when hide $ putChar '\n'
  return inp

upHaddockCmd :: Bool -> Bool -> IO ()
upHaddockCmd publish force = do
  needProgram "cabal"
  userpassBS <- getUserPassword
  pkgid <- getPackageId
  when force $ do
    let tarball = "dist" </> showPkgId pkgid ++ '-' : "docs" <.> tarGzExt
    exists <- doesFileExist tarball
    when exists $
      removeFile tarball
  _out <- P.readProcessInterleaved_ (P.setStdin (P.byteStringInput userpassBS) $ P.proc "cabal" ("upload" : "--documentation" : ["--publish" | publish]))
  putStrLn $ (if publish then "Published at" else "Uploaded to") +-+ "https://hackage.haskell.org/package/" ++ showPkgId pkgid ++ if publish then "" else "/candidate"

cabal_ :: String -> [String] -> IO ()
cabal_ c args =
  cmd_ "cabal" (c:args)

withTempDirectory :: FilePath -> IO a -> IO a
withTempDirectory dir run =
  bracket_ (createDirectory dir) (removeDirectoryRecursive dir) $
  withCurrentDirectory dir run

-- #if (defined(MIN_VERSION_simple_cmd) && MIN_VERSION_simple_cmd(0,2,1))
-- #else
needProgram :: String -> IO ()
needProgram prog = do
  mx <- findExecutable prog
  unless (isJust mx) $ error' $ "program needs" +-+ prog
-- #endif

-- FIXME warning if upstream template changed (keep a versioned template copy)
-- FIXME add default templates dir
newCmd :: Maybe String -> Maybe String -> IO ()
newCmd mlicense mproject = do
  needProgram "cabal"
  name <- case mproject of
    Just ns -> return ns
    Nothing -> do
      files <- listDirectory "."
      if null files then takeFileName <$> getCurrentDirectory
        else do
        -- filter out dirs
        dirs <- filterM doesDirectoryExist files
        if dirs == files then error' "Could not guess name: subdirectories found"
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
      cabalversion <- readVersion <$> cmd "cabal" ["--numeric-version"]
      let license =
            case mlicense of
              Just l -> l
              Nothing ->
                if cabalversion > makeVersion [3,4]
                then "BSD-3-Clause"
                else "BSD3"
      cabal_ "init" ["--quiet", "--no-comments", "--non-interactive", "--is-libandexe", "--cabal-version=1.18", "--license=" ++ license, "--package-name=" ++ name, "--version=0.1.0", "--dependency=base<5", "--source-dir=src"]
      whenJustM (cmdMaybe $ P.proc "find" ["-name", "Main.hs"]) $ \ file -> do
        sed ["1s/^module Main where/-- SPDX-License-Identifier:" +-+ license ++ "\\n\\nmodule Main (main) where/"] file
        unless (file == "src/Main.hs") $ renameFile file "src/Main.hs"
      whenM (doesFileExist "CHANGELOG.md") $
        renameFile "CHANGELOG.md" "ChangeLog.md"
      unlessM (doesFileExist "README.md") $
        writeFile "README.md" $ "#" +-+ name ++ "\n"
      unless origsetup $ do
        setup <- doesFileExist setupFile
        when setup $ removeFile setupFile
      setupCabalTemplate name license
    Just cblName ->
      when (cblName /= name) $
      putStrLn $ "Warning:" +-+ cblName +-+ "different to" +-+ name
  haveStackCfg <- doesFileExist "stack.yaml"
  mstack <- findExecutable "stack"
  -- FIXME add stack.yaml template too
  when (not haveStackCfg && isJust mstack) $ do
    -- FIXME determine last lts automatically
    cmd_ "stack" ["init", "--verbosity", "warn", "--resolver", "lts-21"]
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

    setupCabalTemplate :: String -> String -> IO ()
    setupCabalTemplate name license = do
      userTemplate <- getUserConfigFile "hkgr" "template.cabal"
      unlessM (doesFileExist userTemplate) $ do
        origTemplate <- getDataFileName "template.cabal.tmpl"
        createDirectoryIfMissing True $ takeDirectory userTemplate
        -- FIXME put a copy of the current template there too for reference
        copyFile origTemplate userTemplate
        username <- cmdOut $ git "config" ["--global", "user.name"]
        replaceHolder "NAME" username userTemplate
        usermail <-
          fromMaybeM (cmdOut $ git "config" ["--global", "user.email"]) $
            cmdMaybe $ git "config" ["--global", "github.email"]
        replaceHolder "EMAIL" usermail userTemplate
        githubuser <- cmdOut $ git "config" ["--global", "github.user"]
        replaceHolder "USER" githubuser userTemplate
        putStrLn $ userTemplate +-+ "set up"
      copyFile userTemplate $ name <.> "cabal"
      replaceHolder "PROJECT" name $ name <.> "cabal"
      replaceHolder "PROJECT_" (map underscore name) $ name <.> "cabal"
      replaceHolder "SUMMARY" (name +-+ "project") $ name <.> "cabal"
      replaceHolder "LICENSE" license $ name <.> "cabal"
      year <- cmdOut (P.proc "date" ["+%Y"])
      replaceHolder "YEAR" year $ name <.> "cabal"
      let modulePath = "src/MyLib.hs"
      unlessM (doesFileExist modulePath) $ do
        createDirectoryIfMissing True $ takeDirectory modulePath
        writeFile modulePath "-- SPDX-License-Identifier: BSD-3-Clause\n\nmodule MyLib where\n"
      where
        replaceHolder lbl val =
          sed ["s/@" ++ lbl ++ "@/" ++ val ++ "/"]

        underscore '-' = '_'
        underscore c = c

sed :: [String] -> FilePath -> IO ()
sed [] _ = error' "sed given no script"
sed args file =
  cmd_ "sed" $ ["-i", "-e"] ++ intersperse "-e" args ++ [file]

#if !MIN_VERSION_filepath(1,4,2)
isExtensionOf :: String -> FilePath -> Bool
isExtensionOf ext@('.':_) = isSuffixOf ext . takeExtensions
isExtensionOf ext         = isSuffixOf ('.':ext) . takeExtensions
#endif

#if !MIN_VERSION_extra(1,6,15)
fromMaybeM :: Monad m => m a -> m (Maybe a) -> m a
fromMaybeM n = maybeM n return
#endif

-- FIXME rename github remote too?
renameCmd :: String -> IO ()
renameCmd newname = do
  pkgid <- getPackageId
  let oldname = unPackageName (pkgName pkgid)
  sed ["s/" ++ oldname ++ "/" ++ newname ++ "/g", "s/Paths_" ++ replace "-" "_" oldname ++ "/Paths_" ++ replace "-" "_" newname ++ "/g"] $ oldname <.> "cabal"
  git_ "mv" [oldname <.> "cabal", newname <.> "cabal"]
  -- FIXME for all *.hs
  sed ["s/" ++ oldname ++ "/" ++ newname ++ "/g", "s/Paths_" ++ replace "-" "_" oldname ++ "/Paths_" ++ replace "-" "_" newname ++ "/g"] "src/Main.hs"
  sed ["s/" ++ oldname ++ "/" ++ newname ++ "/g"] "README.md"
  sed ["s/" ++ oldname ++ "/" ++ newname ++ "/g"] "ChangeLog.md"
  renameDirectory (".." </> oldname) (".." </> newname)

githubCmd :: IO ()
githubCmd = do
  ghuser <- cmdOut $ git "config" ["--global", "github.user"]
  pkgid <- getPackageId
  let name = unPackageName (pkgName pkgid)
  git_ "remote" ["add", "origin", "git@github.com:" ++ ghuser </> name <.> "git"]
  git_ "branch" ["-M", "main"]
--  git_ "push" ["-u", "origin", "main"]

-- pristine cabal sdist/build fail if there's a top cabal.project
addCabalProject :: IO ()
addCabalProject = do
  let cabalproject = "cabal.project"
  unlessM (doesFileExist cabalproject) $
    writeFile cabalproject "packages: .\n"

pristineBuildCmd :: IO ()
pristineBuildCmd = do
  needProgram "cabal"
  pkgid <- getPackageId
  cwd <- getCurrentDirectory
  let tarball = cwd </> sdistDir </> showPkgId pkgid <.> tarGzExt
  exists <- doesFileExist tarball
  unless exists $
    error' $ "Please 'tagdist' first:" +-+ tarball +-+ "not found"
  withTempDirectory "tmp-build" $ do
    cmd_ "tar" ["xf", tarball]
    setCurrentDirectory $ showPkgId pkgid
    addCabalProject
    putStrLn $ "cabal building" +-+ showPkgId pkgid <.> tarGzExt
    cabal_ "build" []
