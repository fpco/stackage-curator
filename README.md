## stackage-curator

[![Build Status](https://travis-ci.org/fpco/stackage-curator.svg?branch=master)](https://travis-ci.org/fpco/stackage-curator)

This repository contains the code for curating Stackage package sets and
building reusable package databases. It was originally simply called the
stackage package and was part of the stackage repository, but since this is a
tool very few people need to use, we split it into its own package with a name
to indicate it's limited usage (curators only).

More information on Stackage:

* [Stackage Homepage](https://www.stackage.org)
* [Main stackage repo](https://github.com/fpco/stackage)

### Code explanation

We start off with *constraints*. Constraints state things like "package X has a
given version range," who the maintainer is for a package, the description of
the system/compiler being used, etc. `BuildConstraints` describes the build as
a whole, whereas `PackageConstraints` describes the constraints on an
individual package.

There are two primary ways of getting a `BuildConstraints`.
`defaultBuildConstraints` inspects the first GHC in the PATH environment variable to
determine GHC version, core packages, core tools, etc. It then uses the
`Stackage.Config` module to extract information on additional packages to be
installed. The secondary approach is in `Stackage2.UpdateBuildPlan`, which will be
discussed later.

`BuildConstraints` does not specify a build completely. That is given by a
`BuildPlan`, which is similarly broken down into `BuildPlan` and `PackagePlan`.
In order to get a `BuildPlan`, we need two pieces of information: the
`BuildConstraints`, and a package index. The package index (usually downloaded
from Hackage) is a collection of all of the cabal files available.

By applying a `BuildConstraints` to a package index (via `newBuildPlan`), we
get a proposed `BuildPlan`. There is no guarantee that this `BuildPlan` is
valid. To validate it, we use `checkBuildPlan`. A `BuildPlan` is an instance of
both `ToJSON` and `FromJSON`, and therefore can be serialized to a file for
later use.

When dealing with LTS Haskell, we want to be able to take a `BuildPlan`, and
update to a newer `BuildPlan` that keeps all packages at the same major
version.  `updateBuildConstraints` turns a `BuildPlan` into a new
`BuildConstraints` with that restriction, and `updateBuildPlan` applies
`newBuildPlan` to that result. As mentioned previously: this is *not* a
validated result, and therefore `checkBuildPlan` must be used.

A `BuildPlan` can be acted on. This is done to check that all packages compile
together, run relevant test suites, test Haddock documentation is correct, and
produce as artifacts both a self-contained GHC binary package database and a
set of Haddock documentation. (Not yet implemented.)

A `BuildPlan` may be converted into a bundle to be uploaded to Stackage Server.
(Not yet implemented.)
