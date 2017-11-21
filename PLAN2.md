# stackage-curator 2.0 plans

This lists the plans for Stackage Curator 2.0. The main goal: push off
as much of the build work as possible to Stack, which is a far more
well tested code path. That will decrease code duplication here, and
help avoid bugs like incorrect dirtiness detection.

## Generate simple plans

The goal here is to convert either `build-constraints.yaml` (for
nightly and LTS-X.0) or `lts-x.y.yaml` (for LTS point releases) into a
simple plan format. This format is supposed to be consumable directly
by Stack (in other words: a `stack.yaml` file). It will not have all
of the extra metadata currently in plan files (like module
names). That will come later.

There will be some new fields in these simple plans, like identifying
Haddocks as expected failures, or indicating whether tests should be
skipped.

## Teach Stack to build these simple plans

Currently, there's no logic in Stack to "build a `stack.yaml`."
Furthermore, there's no logic to know that some things are allowed to
fail. Stack needs to gain that functionality. It might make the most
sense to expose this at the API level only, and have stackage-curator
submodule in Stack and use its library directly.

## Generate complete plan files

The nightly-YYYY-MM-DD.yaml and lts-x.y.yaml files are still needed:
they contain extra metadata used by stackage-server and likely other
tools, and Stack still uses these files (though it may choose not to
in the future). This step will take the simple plans and generate the
complete plans.

## Bundle and upload

We should generate exactly the same bundle, docmap, plan, etc files as
are generated now, and upload them to the same places. We may also
want to create some new repos for the simple plan files. This may be a
natural time to migrate over to `commercialhaskell/lts-haskell` and
`commercialhaskell/stackage-nightly`.

We can elect in the future to change the upload format, but it should
_not_ occur at the same time as this changeover. We want to keep
backwards compatibility as much as possible.

## Change stackage build scripts

Inside the stackage repo are a set of shell scripts to perform
builds. These will be the last thing to be updated. Since the
generated executables here will have different names and URLs from
those on the master branch, we can continue using both sets of scripts
in parallel while testing out this branch.
