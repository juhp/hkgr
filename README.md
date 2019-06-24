# Hkgr

[![Hackage](https://img.shields.io/hackage/v/hkgr.svg)](https://hackage.haskell.org/package/hkgr)
[![GPL-3 license](https://img.shields.io/badge/license-GPL--3-blue.svg)](LICENSE)
[![Build status](https://secure.travis-ci.org/juhp/hkgr.svg)](https://travis-ci.org/juhp/hkgr)

`hkgr` (pronounced "Hackager") is a tool for making releases of
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

## Example
```
$ git commit -m "new release"
$ git push
$ hkgr tagdist
$ hkgr upload
$ hkgr upload-haddock
$ hkgr publish
$ hkgr publish-haddock
```
