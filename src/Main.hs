{-# LANGUAGE CPP #-}

module Main (main) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (pure, (<$>))
#endif

import Control.Exception (bracket_, finally, onException)
import Control.Monad.Extra
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char
import Data.List.Extra
import Data.Maybe
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import SimpleCabal
import SimpleCmdArgs
import System.Directory
import System.Environment.XDG.BaseDir
import System.Exit (ExitCode (..))
import System.FilePath
import System.IO (BufferMode(NoBuffering), hSetBuffering, hSetEcho,
                  stdin, stdout)
import qualified System.Process.Typed as P
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
      tagDistCmd False <$> forceOpt "Move existing tag"
    , Subcommand "dist" "'cabal sdist' tarball for existing tag" $
      tagDistCmd True <$> forceOpt "Recreate tarball"
    , Subcommand "upload" "'cabal upload' candidate tarball to Hackage" $
      uploadCmd False <$> switchWith 'T' "existing-tag" "Use existing tag to create tarball" <*> forceOpt "Move existing tag"
    , Subcommand "publish" "Publish to Hackage ('cabal upload --publish')" $
      pure $ uploadCmd True False False
    , Subcommand "upload-haddock" "Upload candidate documentation to Hackage" $
      pure $ upHaddockCmd False
    , Subcommand "publish-haddock" "Publish documentation to Hackage" $
      pure $ upHaddockCmd True
    , Subcommand "version" "Show the package version from .cabal file" $
      pure showVersionCmd
    , Subcommand "rename" "Rename the Cabal package" $
      renameCmd <$> strArg "NEWNAME"
    ]
  where
    forceOpt = switchWith 'f' "force"

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

-- cmd :: String -> [String] -> IO String
-- cmd c args =
--   B.unpack . removeTrailingNewline <$> P.readProcessStdout_ (P.proc c args)

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

tagDistCmd :: Bool -> Bool -> IO ()
tagDistCmd existingtag force = do
  pkgid <- checkPackage True
  let tag = pkgidTag pkgid
  tagHash <- cmdMaybe (git "rev-parse" [tag])
  if existingtag
    then do
    when (isNothing tagHash) $
      error' $ "tag " ++ tag ++ " do not exist"
    sdist force pkgid
    else do
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
        error' $ ".cabal filename differs from package name: " ++ unPackageName pkg
      diff <- cmdOut (git "diff" ["-U0", "HEAD", cabalfile])
      when (any ("+version:" `isPrefixOf`) (lines (map toLower diff))) $
        error' "Please commit or revert the changed package Version first"

    checkNotPublished :: PackageIdentifier -> IO ()
    checkNotPublished pkgid = do
      let published = sdistDir </> showPkgId pkgid <.> ".tar.gz" <.> "published"
      exists <- doesFileExist published
      when exists $ error' $ showPkgId pkgid <> " was already published!!"
      let oldpublished = "dist" </> showPkgId pkgid <.> ".tar.gz" <.> "published"
      oldExists <- doesFileExist oldpublished
      when oldExists $ error' $ showPkgId pkgid <> " was already published!!"

sdistDir :: FilePath
sdistDir = ".hkgr"

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
    whenM (doesFileExist ".gitmodules") $
      git_ "submodule" ["update", "--init"]
    cabal_ "check" []
    mhlint <- findExecutable "hlint"
    when (isJust mhlint) $ do
      putStrLn "Running hlint"
      void $ P.runProcess $ P.proc "hlint" ["--no-summary", "."]
    let dest = takeDirectory $ cwd </> target
    unlessM (doesDirectoryExist dest) $
      createDirectoryIfMissing True dest
    cabal_ "v2-sdist" ["--output-dir=" ++ dest]

showVersionCmd :: IO ()
showVersionCmd = do
  pkgid <- getPackageId
  putStrLn $ packageVersion pkgid

-- FIXME cabal install creates tarballs now
uploadCmd :: Bool -> Bool -> Bool -> IO ()
uploadCmd publish existingtag force = do
  needProgram "cabal"
  pkgid <- checkPackage False
  let file = sdistDir </> showPkgId pkgid <.> ".tar.gz"
      tag = pkgidTag pkgid
  exists <- doesFileExist file
  when (force || not exists) $
    tagDistCmd existingtag force
  whenM (null <$> cmdOut (git "branch" ["--contains", "tags/" ++ tag])) $
    error' $ tag ++ " is no longer on branch: use --force to move it"
  untagged <- cmdLines $ git "log" ["--pretty=reference", tag ++ "..HEAD"]
  unless (null untagged) $ do
    putStrLn "untagged newer commits:"
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
  userpassBS <- getUserPassword
  void $ P.readProcessInterleaved_ (P.setStdin (P.byteStringInput userpassBS) $ P.proc "cabal" ("upload" : ["--publish" | publish] ++ [file]))
  putStrLn $ (if publish then "Published at " else "Uploaded to ") ++ "https://hackage.haskell.org/package/" ++ showPkgId pkgid ++ if publish then "" else "/candidate"
  when publish $
    createFileLink (takeFileName file) (file <.> "published")

getUserPassword :: IO B.ByteString
getUserPassword = do
  cabalConfig <- do
    home <- getHomeDirectory
    let cabalConfig = home </> ".cabal/config"
    exists <- doesFileExist cabalConfig
    if exists
      then readFile cabalConfig
      else do
      putStrLn $ "Warning: " ++ cabalConfig ++ " not found!"
      return ""
  muser <- maybeGetHackage "username" False cabalConfig
  mpasswd <- maybeGetHackage "password" True cabalConfig
  return $ B.pack $ unlines $ catMaybes [muser, mpasswd]
  where
    maybeGetHackage :: String -> Bool -> String -> IO (Maybe String)
    maybeGetHackage field hide cabalConfig =
      if haveCabalConfigField
        then return Nothing
        else Just <$> prompt hide ("hackage.haskell.org " ++ field)
      where
        haveCabalConfigField :: Bool
        haveCabalConfigField =
          any ((field ++ ":") `isPrefixOf`) (lines cabalConfig)

prompt :: Bool -> String -> IO String
prompt hide s = do
  putStr $ s ++ ": "
  inp <- if hide then withoutEcho getLine else getLine
  when hide $ putChar '\n'
  if null inp
    then prompt hide s
    else return inp
  where
    withoutEcho :: IO a -> IO a
    withoutEcho action =
      finally (hSetEcho stdin False >> action) (hSetEcho stdin True)

upHaddockCmd :: Bool -> IO ()
upHaddockCmd publish = do
  needProgram "cabal"
  userpassBS <- getUserPassword
  _out <- P.readProcessInterleaved_ (P.setStdin (P.byteStringInput userpassBS) $ P.proc "cabal" ("upload" : "--documentation" : ["--publish" | publish]))

  pkgid <- getPackageId
  putStrLn $ (if publish then "Published at " else "Uploaded to ") ++ "https://hackage.haskell.org/package/" ++ showPkgId pkgid ++ if publish then "" else "/candidate"

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
  unless (isJust mx) $ error' $ "program needs " ++ prog
-- #endif

-- FIXME warning if upstream template changed (keep a versioned template copy)
-- FIXME add default templates dir
-- FIXME --license
newCmd :: Maybe String -> IO ()
newCmd mproject = do
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
      cabal_ "init" ["--quiet", "--no-comments", "--non-interactive", "--is-libandexe", "--cabal-version=1.18", "--license=BSD3", "--package-name=" ++ name, "--version=0.1.0", "--dependency=base<5", "--source-dir=src"]
      whenJustM (cmdMaybe $ P.proc "find" ["-name", "Main.hs"]) $ \ file -> do
        sed ["1s/^module Main where/-- SPDX-License-Identifier: BSD-3-Clause\\n\\nmodule Main (main) where/"] file
        unless (file == "src/Main.hs") $ renameFile file "src/Main.hs"
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
    cmd_ "stack" ["init", "--verbosity", "warn", "--resolver", "lts-17"]
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

    setupCabalTemplate :: String -> IO ()
    setupCabalTemplate name = do
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
        putStrLn $ userTemplate ++ " set up"
      copyFile userTemplate $ name <.> "cabal"
      replaceHolder "PROJECT" name $ name <.> "cabal"
      replaceHolder "PROJECT_" (map underscore name) $ name <.> "cabal"
      replaceHolder "SUMMARY" (name ++ " project") $ name <.> "cabal"
      year <- cmdOut (P.proc "date" ["+%Y"])
      replaceHolder "YEAR" year $ name <.> "cabal"
      let modulePath = "src/MyLib.hs"
      unlessM (doesFileExist modulePath) $ do
        createDirectoryIfMissing True $ takeDirectory modulePath
        writeFile modulePath "-- SPDX-License-Identifier: BSD-3-Clause\n\nmodule MyLib where\n"
      where
        replaceHolder lbl val file =
          sed ["s/@" ++ lbl ++ "@/" ++ val ++ "/"] file

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

renameCmd :: String -> IO ()
renameCmd newname = do
  pkgid <- getPackageId
  let oldname = unPackageName (pkgName pkgid)
  sed ["s/" ++ oldname ++ "/" ++ newname ++ "/g"] $ oldname <.> "cabal"
  renameFile (oldname <.> "cabal") (newname <.> "cabal")
  -- FIXME adjust README
