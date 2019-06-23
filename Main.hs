{-# LANGUAGE CPP #-}

module Main (main) where

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
import Control.Applicative (pure, (<$>))
#endif

import Control.Exception (bracket_)
import Control.Monad
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
  simpleCmdArgs (Just version) "HacKaGe Release workflow"
  "A tool to help Hackage maintainers with releasing packages" $
  subcommands
  [ Subcommand "tag" "'git tag' version" $
    gitTagCmd <$> forceOpt "Move existing tag"
  , Subcommand "dist" "Make tarball from latest tag ('cabal sdist')" $
    sdistCmd <$> forceOpt "Overwrite existing dist tarball"
  , Subcommand "version" "Show the package version from .cabal file" $
    pure showVersionCmd
  , Subcommand "upload" "'cabal upload' tarball to Hackage" $ pure $ uploadCmd False
  , Subcommand "publish" "Publish to Hackage ('cabal upload --publish')" $
    pure $ uploadCmd True
  , Subcommand "upload-haddock" "Upload documentation to Hackage" $ pure $ upHaddockCmd False
  , Subcommand "publish-haddock" "Upload documentation to Hackage" $ pure $ upHaddockCmd True
  , Subcommand "push-tags" "'git push --tags' to origin" $ pure pushCmd
  ]
  where
    forceOpt = switchWith 'f' "force"

gitTagCmd :: Bool -> IO ()
gitTagCmd force = do
  pkgid <- getPackageId
  checkNotPublished pkgid
  let tag = packageVersion pkgid
  git_ "tag" $ ["--force" | force] ++ [tag]
  unless force $ putStrLn tag

checkNotPublished :: PackageIdentifier -> IO ()
checkNotPublished pkgid = do
  let published = "dist" </> showPkgId pkgid <.> ".tar.gz" <.> "published"
  exists <- doesFileExist published
  when exists $ error' $ showPkgId pkgid <> " was already published!!"

sdistCmd :: Bool -> IO ()
sdistCmd force = do
  pkgid <- getPackageId
  let ver = packageVersion pkgid
  let target = "dist" </> showPkgId pkgid <.> ".tar.gz"
  checkNotPublished pkgid
  haveTarget <- doesFileExist target
  if haveTarget
    then if force
         then removeFile target
         else error' $ target <> " exists already!"
    else when force $ error' "Target does not exist, please use 'dist' command"
  haveTag <- pipeBool ("git", ["tag"]) ("grep",["-q",ver])
  unless haveTag $ gitTagCmd False
  cwd <- getCurrentDirectory
  withTempDirectory "tmp-sdist" $ do
    git_ "clone" ["-q", "--no-checkout", "..", "."]
    git_ "checkout" ["-q", ver]
    cabal_ "check" []
    cabal_ "configure" []
    -- cabal_ "build" []
    cmd_ "hlint" ["."]
    cabal_ "sdist" []
    renameFile target (cwd </> target)

showVersionCmd :: IO ()
showVersionCmd = do
  pkgid <- getPackageId
  putStrLn $ packageVersion pkgid

uploadCmd :: Bool -> IO ()
uploadCmd publish = do
  pkgid <- getPackageId
  checkNotPublished pkgid
  let file = "dist" </> showPkgId pkgid <.> ".tar.gz"
  cabal_ "upload" $ ["--publish" | publish] ++ [file]
  when publish $ do
    git_ "push" [tag]
    createFileLink file (takeFileName file <.> "published")

pushCmd :: IO ()
pushCmd =
  git_ "push" ["--tags"]

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
