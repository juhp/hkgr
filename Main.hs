{-# LANGUAGE CPP #-}

module Main (main) where

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
import Control.Applicative (pure, (<$>))
#endif

import Control.Exception (bracket_, onException)
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,11,0))
#else
import Data.Semigroup ((<>))
#endif
import SimpleCabal
import SimpleCmd
import SimpleCmd.Git
import SimpleCmdArgs
import System.Directory
import System.FilePath
import Paths_hkgr (version)

main :: IO ()
main =
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
    cabal_ "v2-sdist" ["--output-dir=" ++ dest ]

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
  cabal_ "upload" $ ["-v0"] ++ ["--publish" | publish] ++ [file]
  putStrLn $ (if publish then "Published at " else "Uploaded to ") ++ "https://hackage.haskell.org/package/" ++ showPkgId pkgid ++ if publish then "" else "/candidate"
  when publish $
    createFileLink (takeFileName file) (file <.> "published")

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
-- FIXME --lib --exe
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
          else do
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
      cabal_ "init" ["--minimal", "--non-interactive"]
      -- FIXME setup better default .cabal template
      unless origsetup $ do
        setup <- doesFileExist setupFile
        when setup $ removeFile setupFile
    -- FIXME warn if name different
    Just _cbl -> return ()
  haveStack <- doesFileExist "stack.yaml"
  unless haveStack $ do
    cmd_ "stack" ["init"] -- FIXME remove comments
    cmd_ "sed" ["-i", "-e", "/^#/d", "-e", "/^$/d", "stack.yaml"]
  haveGit <- doesDirectoryExist ".git"
  unless haveGit $ git_ "init" []
  where
    checkForCabalFile :: IO (Maybe String)
    checkForCabalFile = do
      files <- listDirectory "."
      case filter ("cabal" `isExtensionOf`) files of
        [] -> return Nothing
        [cbl] -> return $ Just (takeBaseName cbl)
        _ -> error' "More than one .cabal file found!"

#if (defined(MIN_VERSION_filepath) && MIN_VERSION_filepath(1,4,2))
#else
    isExtensionOf :: String -> FilePath -> Bool
    isExtensionOf ext@('.':_) = isSuffixOf ext . takeExtensions
    isExtensionOf ext         = isSuffixOf ('.':ext) . takeExtensions
#endif
