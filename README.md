# Hkgr

[![Hackage](https://img.shields.io/hackage/v/hkgr.svg)](https://hackage.haskell.org/package/hkgr)
[![GPL-3 license](https://img.shields.io/badge/license-GPL--3-blue.svg)](LICENSE)
[![Build status](https://secure.travis-ci.org/juhp/hkgr.svg)](https://travis-ci.org/juhp/hkgr)

`hkgr` (pronounced "hackager") is a tool for making releases of
Haskell packages on Hackage.

## Usage

```
$ hkgr
HacKaGe Release workflow

Usage: hkgr [--version] COMMAND
  A tool to help Hackage maintainers with releasing packages

Available options:
  -h,--help                Show this help text
  --version                Show version

Available commands:
  tagdist                  'git tag' version and 'cabal sdist' tarball
  upload                   'cabal upload' candidate tarball to Hackage
  publish                  Publish to Hackage ('cabal upload --publish')
  upload-haddock           Upload candidate documentation to Hackage
  publish-haddock          Publish documentation to Hackage
  version                  Show the package version from .cabal file
```

## Explanation

You are preparing for a release...

So you commit changes to your package and push them to check CI results:

```
$ git commit -m "new release"
$ git push
```


### tagdist
Now you want to make a dist tarball, which is done from a git tag:
```
$ hkgr tagdist
```
The `tagdist` command reads the current package version
(from the `.cabal` file in the current directory) to `git tag`,
and then runs `cabal sdist` on a temporary pristine checkout of that tag.

If the tag already exists
(eg if you already ran `tagdist` earlier for the current version),
you can use `--force` to move the tag to the latest commit
and generate a new tarball off that,
otherwise `tagdist` will refuse to run again.

One should not be able to (force) `tagdist` for an already published
(ie released) version.

(If sdist fails for some reason then hkgr tries to reset the tag.)

### upload
If all is good, it's time for a candidate release:

```
$ hkgr upload
```
This uploads a candidate dist tarball to Hackage: this can be repeated.

Haddock draft documentation can also be uploaded if desired:
```
$ hkgr upload-haddock
```

### publish
Once you are happy, you can release to Hackage:

```
$ hkgr publish
```

If it succeeds then hkgr creates a "published lockfile" in `dist/`,
and the git tag gets pushed to origin.

(Then hkgr will refuse to do commands for the released version.)

Optionally one can publish haddock docs:
```
$ hkgr publish-haddock
```

## Requirements

hkgr uses `cabal-install`, `git`, and also `hlint` if available.
