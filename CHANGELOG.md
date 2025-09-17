# Changelog

## 0.5 (2025-09-18)
- support Cabal's XDG layout (#5, reported by Artem)
  ie check for ~/.config/cabal/config before ~/.cabal/config
- do not prompt for password if there is is 'token' or 'password-command' (#5)
- upload-haddock/publish-haddock: now builds docs first with v2-haddock (#6)
- bump default stack init to lts-23

## 0.4.8 (2025-05-23)
- bump stack default to lts-22

## 0.4.7 (2025-03-11)
- 'rename': only sed Main.hs if it exists
- 'sdist': prefix "Running hlint" with '#'
- 'upload': handle existing tarball from aborted build without tag
- 'upload-haddock': add --force which removes existing haddock tarball

## 0.4.6 (2024-06-20)
- 'new': add --license option
- 'new': bump stack.yaml resolver to lts-21
- add missing SUMMARY placeholder to template

## 0.4.5 (2024-05-23)
- 'tagdist', 'upload': also do pristine cabal build unless --no-build

## 0.4.4 (2024-05-03)
- .cabal template: add autogen-modules and update tested-with ghc versions
- publish,build: output message before cabal build
- use simple-cmd +-+

## 0.4.3.2 (2023-10-20)
- use a cabal.project for pristine sdist/build to ignore any parent project
- .cabal template: updated tested-with ghc versions

## 0.4.3.1 (2023-07-05)
- fix the template file SPDX license tag and cabal-version field placement

## 0.4.3 (2023-05-30)
- bump template to Cabal 2.2
- add pristine 'build' command: used before publish
- use haskeline for reading user/passwd via simple-prompt

## 0.4.2 (2022-06-24)
- 'new': need cabal init --license option, otherwise no LICENSE file is created
- 'github': new command to add github remote for project

## 0.4.1 (2022-06-23)
- 'rename' improvements
- 'new': avoid error on cabal >= 3.4 by not passing --license to cabal init

## 0.4 (2022-03-27)
- template.cabal: use cabal 2.0 and define hs-source-dirs
- 'tagdist --existing-tag' replaces 'dist' command
- 'upload': add --existing-tag for dist rather than tagdist
- 'new': stack init with lts-17
- 'rename': experimental command to rename a project
- add '--no-hlint' option to skip running hlint
  (also hlint is no longer run for 'publish')

## 0.3 (2020-05-07)
- only read Hackage username/password if not in ~/.cabal/config
- 'upload': show newer untagged commits
- 'dist': new command for a manually tagged release
- fix the check for package version committed

## 0.2.7 (2020-02-27)
- Main.hs: explicitly export main and add SPDX-License-Identifier
- move Main.hs to src/
- handle git submodules (not --recursive yet)
- use typed-process to interleave IO to display auth errors
- check name and .cabal filename consistent
- put tarballs in .hkgr/
- upload: error if tag no longer on branch
- publish: only push up to tag

## 0.2.6.1 (2020-07-30)
- upload: do not hide output since it conceals any error
- new: improvements to work better with cabal-3.0 init

## 0.2.6 (2020-06-11)
- tagdist: include existing tag in error message
- experimental 'new' project command with user template file
  `~/.config/hkgr/template.cabal`
- switch to cabal v2-sdist
- upload: display error correctly
- upload: add --force switch - to refresh tag and tarball

## 0.2.5.2 (2020-02-29)
- no hlint summary
- fix published message

## 0.2.5.1 (2020-02-29)
- improve output for hlint and uploaded url
- also check for staged changes

## 0.2.5 (2020-02-29)
- check that package version is committed
- use quiet cabal v1-configure and v1-sdist commands
- make cabal upload quiet

## 0.2.4.1 (2020-02-11)
- assert that cabal-install installed
- run hlint in git checkout instead of working tree

## 0.2.4 (2019-10-05)
- git push before publishing
- only push up to tag

## 0.2.3 (2019-09-30)
- push git tag
- hlint is now a warning not an error
- prefix version tags with v
- surround git diff output with quote lines
- catch exception for sdist

## 0.2.2 (2019-07-06)
- run hlint before git tag
- relax force sdist when no existing tarball
- show git diff to warn project dirty

## 0.2.1 (2019-06-24)
- fix creation of published symlink lockfile
- tagdist before `cabal upload` if no tarball

## 0.2 (2019-06-24)
- merge tag and dist commands into tagdist
- if sdist fails then reset tag
- drop push-tags command

## 0.1 (2019-06-24)
- add published lock file: prevents tagging/dist/upload after publish
- tag before sdist if no tag
- push tag after publishing

## 0.0 (2019-06-08)
- Initially created.
