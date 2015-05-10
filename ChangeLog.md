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
