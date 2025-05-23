# Hkgr

[![Hackage](https://img.shields.io/hackage/v/hkgr.svg)](https://hackage.haskell.org/package/hkgr)
[![GPL-3 license](https://img.shields.io/badge/license-GPL--3-blue.svg)](LICENSE)
[![GitHub CI](https://github.com/juhp/hkgr/workflows/build/badge.svg)](https://github.com/juhp/hkgr/actions)

`hkgr` (pronounced "hackager") is a tool for making releases of
Haskell packages on Hackage.

It uses a cautious stepped iterative approach to releases.

## Example usage

Here is an example of doing a release of hkgr itself.

After committing the latest changes for the release, create a tag and tarball:

```shellsession
$ hkgr tagdist
v0.4
No errors or warnings could be found in the package.
Running hlint
src/Main.hs:(407,9)-(408,55): Warning: Eta reduce
Found:
  replaceHolder lbl val file
    = sed ["s/@" ++ lbl ++ "@/" ++ val ++ "/"] file
Perhaps:
  replaceHolder lbl val = sed ["s/@" ++ lbl ++ "@/" ++ val ++ "/"]

[]
["src/Main.hs","data/template.cabal.tmpl","README.md","CHANGELOG.md","LICENSE","hkgr.cabal"]
[(NoExec,"CHANGELOG.md"),(NoExec,"LICENSE"),(NoExec,"README.md"),(NoExec,"data/template.cabal.tmpl"),(NoExec,"hkgr.cabal"),(NoExec,"src/Main.hs")]
Wrote tarball sdist to /home/petersen/github/hkgr/.hkgr/hkgr-0.4.tar.gz
hackage.haskell.org password: ^C
```

After fixing up, retag a new tarball with `--force` and upload candidate,
in one go:

```shellsession
$ hkgr upload -f
Updated tag 'v0.4' (was f6d72ba)
No errors or warnings could be found in the package.
Running hlint
[]
["src/Main.hs","data/template.cabal.tmpl","README.md","CHANGELOG.md","LICENSE","hkgr.cabal"]
[(NoExec,"CHANGELOG.md"),(NoExec,"LICENSE"),(NoExec,"README.md"),(NoExec,"data/template.cabal.tmpl"),(NoExec,"hkgr.cabal"),(NoExec,"src/Main.hs")]
Wrote tarball sdist to /home/petersen/github/hkgr/.hkgr/hkgr-0.4.tar.gz
hackage.haskell.org password:
Uploaded to https://hackage.haskell.org/package/hkgr-0.4/candidate
```

Alternatively if you had manually tagged the release with `v0.4`
you can use `hkgr tagdist --existing-tag` to create a dist tarball.

One can continue to `tagdist -f` and/or `upload -f` until
everything looks good and CI passed etc.

Then it is time to push the final tag and publish the release:

```shellsession
$ hkgr publish
git pushing... done
Total 0 (delta 0), reused 0 (delta 0), pack-reused 0
To github.com:juhp/hkgr.git
 * [new tag]         v0.4 -> v0.4
hackage.haskell.org password:
Published at https://hackage.haskell.org/package/hkgr-0.4
```

## Help

`$ hkgr --version`

```
0.4.8
```
`$ hkgr --help`

```
Hackage Release tool

Usage: hkgr [--version] COMMAND

  'Hackager' is a package release tool for easy Hackage workflow

Available options:
  -h,--help                Show this help text
  --version                Show version

Available commands:
  new                      setup a new project
  tagdist                  'git tag' version and 'cabal sdist' tarball
  upload                   'cabal upload' candidate tarball to Hackage
  publish                  git push and cabal upload --publish
  upload-haddock           Upload candidate documentation to Hackage
  publish-haddock          Publish documentation to Hackage
  build                    Do a local pristine build from the tarball
  version                  Show the package version from .cabal file
  rename                   Rename the Cabal package
  github                   Add github repo
```

## Details

### tagdist
`hkgr tagdist` creates a git tag and a sdist tarball from it:

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
(ie released) version made with hkgr, until the version is bumped.

If sdist fails for some reason then hkgr tries to reset the tag.

A cabal build of the tarball content is also attempted to catch any build errors
from the package.

Alternatively if you have already manually tagged a release with 'v' prefix
you can use `--existing-tag` to create a dist tarball.

### upload
`hkgr upload` uploads the tarball to Hackage as a candidate release.
Like `hkgr tagdist -f`, `hkgr upload -f` can be repeated.

Haddock draft documentation can also be uploaded once if desired
with `hkgr upload-haddock`.

If you have an existing version tag (starting with `v`) you can use
the `--existing-tag` option to skip the tagging step (like for `tagdist`).

### publish
`hkgr publish` releases the tarball to Hackage, after doing a pristine
local build and git pushing the tag and its commits to origin.

If it succeeds, then hkgr creates a "published lockfile" in `.hkgr/`.

(Then hkgr will refuse to do further commands on the released version.)

Optionally one can publish haddock docs with `hkgr publish-haddock`.

### build
`hkgr build` will try to do a pristine build of the latest created tarball
for the tag. This is useful for catching missing files from the tarball,
preventing brownbag releases.

### new
`hkgr new` creates a new project.

If you don't pass a name it will try to check and use the current directory name.

It uses `cabal init` to setup various files but replaces the .cabal file
with a template stored in `~/.config/hkgr/template.cabal` which the user
can freely customize.

A `stack.yaml` file and git repo is also set up.

### github
(One can use `gh repo create` etc to create the project repo on Github)
and then `hkgr github` to add the github remote to your project.

## Requirements
hkgr uses `cabal-install` >=2, `git`, and also `hlint` if available.

## Contribute
`hkgr` is licensed and distributed under the GPL version 3 or later.

Reports and contributions are welcome at <https://github.com/juhp/hkgr>.
