# Changelog

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
* Initially created.
