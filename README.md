# Package-Ops

Automate Haskell packaging.

This package contains:

    - A library of functions for working with .cabal files & git
    - A few opinionated command-line tools that implement the author's most frequent packaging tasks.
    
The library is intended to be generally useful.  The executables
aren't, necessarily.

## cabal-sandbox-clean

Delete a cabal sandbox in the current working directory, and restore,
preserving add-source dependencies.

## point-release

Automate releases following a new major version of a dependency.

    - new upper bounds in `.cabal`
    - update Changelog
    - increment package version
    - git commits for the above
