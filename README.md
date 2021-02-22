# Hkgr

[![Hackage](https://img.shields.io/hackage/v/hkgr.svg)](https://hackage.haskell.org/package/hkgr)
[![GPL-3 license](https://img.shields.io/badge/license-GPL--3-blue.svg)](LICENSE)
[![GitHub CI](https://github.com/juhp/hkgr/workflows/build/badge.svg)](https://github.com/juhp/hkgr/actions)

`hkgr` (pronounced "hackager") is a tool for making releases of
Haskell packages on Hackage.

It uses a cautious stepped approach to releases.

## Example usage

Here is an example of doing a release of hkgr itself.

After committing the latest changes for the release, create a tag and tarball:

```
$ hkgr tagdist
v0.2.5
No errors or warnings could be found in the package.
Running hlint
./Main.hs:107:28: Warning: Redundant do
Found:
  do void $ cmdBool "hlint" ["."]
Perhaps:
  void $ cmdBool "hlint" ["."]
Resolving dependencies...
Configuring hkgr-0.2.5...
Building source dist for hkgr-0.2.5...
Preprocessing executable 'hkgr' for hkgr-0.2.5..
Source tarball created: dist/hkgr-0.2.5.tar.gz
```

After fixing up, retag a new tarball:

```
$ hkgr tagdist -f
Updated tag 'v0.2.5' (was 55b69db)
No errors or warnings could be found in the package.
Running hlint
Resolving dependencies...
Configuring hkgr-0.2.5...
Building source dist for hkgr-0.2.5...
Preprocessing executable 'hkgr' for hkgr-0.2.5..
Source tarball created: dist/hkgr-0.2.5.tar.gz
```

The tarball can now be uploaded to Hackage as a candidate release:

```
$ hkgr upload

Uploaded to https://hackage.haskell.org/package/hkgr-0.2.5/candidate
```

One can continue to `tagdist -f` and `upload` until
everything looks good and CI passed etc,
then it is time to push the final tag and publish the release:

```
$ hkgr publish
Everything up-to-date
Total 0 (delta 0), reused 0 (delta 0)
To github.com:juhp/hkgr.git
 * [new tag]         v0.2.5 -> v0.2.5

Published at https://hackage.haskell.org/package/hkgr-0.2.5
```

## Details

### tagdist
`hkgr tagdist` makes a dist tarball from a git tag:

The `tagdist` command first reads the current package version
(from the `.cabal` file in the current directory), and uses that to `git tag`.
It then runs `cabal sdist` from a temporary pristine checkout of the tag
to generate the dist tarball.

Note that hkgr is lenient: it allows making a release with uncommitted changes
in the working tree, but it will show the uncommitted changes.
However the version must be committed.

If the tag already exists (eg if you already ran `tagdist` earlier),
and you need to add commits to the release
you can use `--force` to move the tag to the latest commit
and generate a new tarball off that,
otherwise `tagdist` refuses to run again to prevent accidently overwriting
the tag and dist tarball.

One should not be able to `tagdist` on an already published
(ie released) version made with hkgr.

If sdist fails for some reason then hkgr tries to reset the tag.

### upload
`hkgr upload` uploads the tarball to Hackage as a candidate release.
Like `hkgr tagdist -f`, this can be repeated.

Haddock draft documentation can also be uploaded if desired with `hkgr upload-haddock`.

### publish
`hkgr publish` releases the tarball to Hackage.

If it succeeds then hkgr creates a "published lockfile" in `dist/`,
and the git tag is pushed to origin.

(Then hkgr will refuse to do further commands on the released version.)

Optionally one can publish haddock docs with `hkgr publish-haddock`.

### new
`hkgr new` creates a new project.

If you don't pass a name it will try to check the current directory.

It uses `cabal init` to setup various files but replaces the .cabal file
with a template stored in `~/.config/hkgr/template.cabal` which the user
can freely customize.

A `stack.yaml` file and git repo is also set up.

One can use `hub create` etc to create the project on Github.

## Requirements

hkgr uses `cabal-install` >=2, `git`, and also `hlint` if available.
