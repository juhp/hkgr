module Main (main) where

import Control.Exception (bracket_)
import Control.Monad
import SimpleCmd
import SimpleCmd.Git
import SimpleCmdArgs
import System.Directory
import System.FilePath
import Paths_hkgrelease (version)

main :: IO ()
main =
  simpleCmdArgs (Just version) "Hackage maintainer release workflow"
  "Helps Hackage package maintainers with releasing packages" $
  subcommands
  [ Subcommand "tag" "'git tag' version" $ pure $ gitTagCmd False
  , Subcommand "dist" "Make tarball from latest tag ('cabal sdist')" $
    pure sdistCmd
  , Subcommand "version" "Show the package version from .cabal file" $
    pure showVersionCmd
  , Subcommand "upload" "'cabal upload' tarball to Hackage" $ pure $ uploadCmd False
  , Subcommand "tag-force" "Update version tag with 'git tag --force'" $
    pure $ gitTagCmd True
  , Subcommand "push-tags" "'git push --tags' to origin" $ pure pushCmd
  , Subcommand "publish" "Publish to Hackage ('cabal upload --publish')" $
    pure $ uploadCmd True
  , Subcommand "upload-haddock" "Upload documentation to Hackage" $ pure $ upHaddockCmd False
  , Subcommand "publish-haddock" "Upload documentation to Hackage" $ pure $ upHaddockCmd True
  ]

getName :: IO String
getName =
  takeBaseName <$> getCurrentDirectory

getCabalFile :: IO FilePath
getCabalFile = do
  name <- getName
  return $ name <.> "cabal"

getVersion :: IO String
getVersion = do
  cbl <- getCabalFile
  cmd "sed" ["-ne", "s/^[Vv]ersion:[[:space:]]*//p", cbl]

getTarfile :: IO FilePath
getTarfile = do
  name <- getName
  ver <- getVersion
  return $ "dist" </> name <> "-" <> ver <.> ".tar.gz"

gitTagCmd :: Bool -> IO ()
gitTagCmd force = do
  ver <- getVersion
  git_ "tag" $ ["--force" | force] ++ [ver]

sdistCmd :: IO ()
sdistCmd = do
  ver <- getVersion
  name <- getName
  let filename = name <> "-" <> ver <.> ".tar.gz"
      target = "dist" </> filename
  haveTarget <- doesFileExist target
  when haveTarget $ error' $ target <> " exists already"
  haveTag <- pipeBool ("git", ["tag"]) ("grep",["-q",ver])
  unless haveTag $ error' $ "Please tag " <> ver <> " first!"
  cwd <- getCurrentDirectory
  withTempDirectory "tmp-sdist" $ do
    git_ "clone" ["..", "."]
    git_ "checkout" ["-q", ver]
    cabal_ "check" []
    cabal_ "configure" []
    -- cabal_ "build" []
    cmd_ "hlint" ["."]
    cabal_ "sdist" []
    renameFile target (cwd </> target)

showVersionCmd :: IO ()
showVersionCmd = do
  ver <- getVersion
  putStrLn ver

uploadCmd :: Bool -> IO ()
uploadCmd publish = do
  file <- getTarfile
  cabal_ "upload" $ ["--publish" | publish] ++ [file]

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
