## 0.14.1

* configure-args
* Support for GHC 8's documentation directory location
* Checked cabal-version in .cabal files

## 0.14.0

* Move stackage-types into this package
* Move stackage-build-plan into this package
* Start building benchmarks [stackage#1372](https://github.com/fpco/stackage/issues/1372)
* Add cabal-from-head
* Include cabal file size and hash info

## 0.13.3

Move away from outdated stackage-metadata

We already have local package index functionality, which uses the
correct index. See:
https://groups.google.com/d/msg/stackage/bf1xewtp9oo/MUB2K20OJQAJ

## 0.13.2

* --no-rebuild-cabal
* Fix allow-newer by simply stripping all version bounds in .cabal files
* Fix build failure [#13](https://github.com/fpco/stackage-curator/issues/13)

## 0.13.1

* Let test suite pass when no package index available [stackage#1165](https://github.com/fpco/stackage/issues/1165)

## 0.13.0

* build-tool-overrides
* Avoid using the cabal-install executable [stackage#1107](https://github.com/fpco/stackage/issues/1107)

## 0.12.0

* New create-plan flags: `--add-package`, `--expect-test-failure` and `--expect-haddock-failure`
* Package description caching

## 0.11.0

* Use newest version of libraries [#6](https://github.com/fpco/stackage-curator/issues/6)

## 0.10.0

* Added `pcSkipBuild`

## 0.8.2

* `upload-docs` command

## 0.8.1

* Redefine core packages [#395](https://github.com/fpco/stackage/issues/395)
* Add --constraint flag for create-plan

## 0.8.0.1

* GHC 7.10 support

## 0.8.0

* Restructured commands to work on server/Docker setup

## 0.7.4

* `-j`/`--jobs` option for build flags
* Only pass in required .haddock files (more memory efficiency)

## 0.7.3

* Number of jobs == number of capabilities
* `--bundle-dest`
* `--skip-git-push`
* Removed some of the old upload stuff
* Better exception output (limited to 500 characters)

## 0.7.2

* Add `diff` command

## 0.7.1.1

* Fix bug with existing .haddock file collection

## 0.7.1

* Add the `stats` command

## 0.7.0.4

* Respect --summary option

## 0.7.0.3

* LTS bumps: specify a goal

## 0.7.0.2

* Deal better with invariant violations around unregistered packages

## 0.7.0

* Renamed to stackage-curator

## 0.6.1

* Switch to V2 upload by default
* --skip-hoogle option

## 0.6.0

* Upload bundle V2 stuff

## 0.5.2

* Upload LTS to Hackage with the name LTSHaskell

## 0.5.1

* `loadBuildConstraints`
* More command line options

## 0.5.0

* Print "Still Alive" while checking, to avoid Travis timeouts
* Include `stackage upload-nightly` command
* Optional plan checking

## 0.4.0

* Command line uses optparse-applicative with additional options
* Library profiling support during build
* Remove cfGlobalFlags (just use package-specific flags)

## 0.3.1

* Added `justCheck` and `stackage check` command line.

## 0.3.0.1

Pre-fetch all packages from Hackage to catch Hackage downtime early.

## 0.3.0.0

* Return progress URL from uploadBundle

## 0.2.1.4

Generate a `core` file in bundles.

## 0.2.1.1

Run postBuild earlier to avoid problems from broken doc uploads.

## 0.2.1.0

* Use TLS manager (to download from Github)

## 0.2.0.0

* Minor fixes
* `pbGlobalInstall`

## 0.1.0.0

First version of Stackage which is made available as its own package. The
codebase has been completely rewritten at this point, to be ready for generated
both Stackage Nightly and LTS Haskell distributions.
