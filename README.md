# hkgr

[![Hackage](https://img.shields.io/hackage/v/hkgr.svg)](https://hackage.haskell.org/package/hkgr)
[![GPL-3 license](https://img.shields.io/badge/license-GPL--3-blue.svg)](LICENSE)
[![Build status](https://secure.travis-ci.org/juhp/hkgr.svg)](https://travis-ci.org/juhp/hkgr)

Publish your package releases on Hackage using hkgr:

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
  tag                      'git tag' version
  dist                     Make tarball from latest tag ('cabal sdist')
  version                  Show the package version from .cabal file
  upload                   'cabal upload' tarball to Hackage
  push-tags                'git push --tags' to origin
  publish                  Publish to Hackage ('cabal upload --publish')
  upload-haddock           Upload documentation to Hackage
  publish-haddock          Upload documentation to Hackage
```

## Example
```
$ git commit -m "new release"
$ hkgr tag
$ hkgr dist
$ hkgr upload
$ git push
$ hkgr push-tags
$ hkgr publish
$ hkgr upload-haddock
$ hkgr publish-haddock
```
