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
  [ Subcommand "tagdist" "'git tag' version and 'cabal sdist' tarball" $
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
  diff <- git "diff" []
  unless (null diff) $ do
    putStrLn "=== start of uncommitted changes ==="
    putStrLn diff
    putStrLn "=== end of uncommitted changes ==="
  pkgid <- checkPackage
  let tag = pkgidTag pkgid
  tagHash <- cmdMaybe "git" ["rev-parse", tag]
  when (isJust tagHash && not force) $
    error' "tag exists: use --force to override"
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
  diff <- git "diff" ["-U0", unPackageName pkg <.> "cabal"]
  when ("version:" `isInfixOf` map toLower diff) $
    error' "Please commit or revert the package Version first"

checkNotPublished :: PackageIdentifier -> IO ()
checkNotPublished pkgid = do
  let published = "dist" </> showPkgId pkgid <.> ".tar.gz" <.> "published"
  exists <- doesFileExist published
  when exists $ error' $ showPkgId pkgid <> " was already published!!"

sdist :: Bool -> PackageIdentifier -> IO ()
sdist force pkgid = do
  let tag = pkgidTag pkgid
  let target = "dist" </> showPkgId pkgid <.> ".tar.gz"
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
    when (isJust mhlint) $ void $ cmdBool "hlint" ["."]
    cabal_ "v1-configure" []
    -- cabal_ "build" []
    cabal_ "v1-sdist" []
    renameFile target (cwd </> target)

showVersionCmd :: IO ()
showVersionCmd = do
  pkgid <- getPackageId
  putStrLn $ packageVersion pkgid

uploadCmd :: Bool -> IO ()
uploadCmd publish = do
  pkgid <- checkPackage
  let file = "dist" </> showPkgId pkgid <.> ".tar.gz"
  exists <- doesFileExist file
  unless exists $ tagDistCmd False
  when publish $ do
    let tag = pkgidTag pkgid
    tagHash <- cmd "git" ["rev-parse", tag]
    branch <- cmd "git" ["branch", "--show-current"]
    git_ "push" ["origin", tagHash ++ ":" ++ branch]
    git_ "push" ["origin", tag]
  cabal_ "upload" $ ["-v0"] ++ ["--publish" | publish] ++ [file]
  putStrLn $ "https://hackage.haskell.org/package/" ++ showPkgId pkgid ++ if publish then "" else "/candidate"
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
