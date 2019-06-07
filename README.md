# hkgrelease

[![Hackage](https://img.shields.io/hackage/v/hkgrelease.svg)](https://hackage.haskell.org/package/hkgrelease)
[![GPL-3 license](https://img.shields.io/badge/license-GPL--3-blue.svg)](LICENSE)
[![Build status](https://secure.travis-ci.org/juhp/hkgrelease.svg)](https://travis-ci.org/juhp/hkgrelease)

Publish your package releases on Hackage using hkgrelease:

## Usage

```
$ hkgrelease
Hackage maintainer release workflow

Usage: hkgrelease [--version] COMMAND
  Helps Hackage package maintainers with releasing packages

Available options:
  -h,--help                Show this help text
  --version                Show version

Available commands:
  tag                      'git tag' version
  dist                     Make tarball from latest tag ('cabal sdist')
  version                  Show the package version from .cabal file
  upload                   'cabal upload' tarball to Hackage
  tag-force                Update version tag with 'git tag --force'
  push-tags                'git push --tags' to origin
  publish                  Publish to Hackage ('cabal upload --publish')
  upload-haddock           Upload documentation to Hackage
  publish-haddock          Upload documentation to Hackage
```

## Example
```
$ git commit -m "new release"
$ hkgrelease tag
$ hkgrelease dist
$ hkgrelease upload
$ git push
$ hkgrelease push-tags
$ hkgrelease publish
$ hkgrelease upload-haddock
$ hkgrelease publish-haddock
```
