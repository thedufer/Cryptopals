Solutions for Matasano's Cryptopals, in Haskell.

Run each solution with `runhaskell
-package-db=.cabal-sandbox/x86_64-osx-ghc-7.8.3-packages.conf.d/ <file>`.
You'll probably have to change that directory depending on your ghc
version/system.  Yeah, there's probably a better way to do this.

Top-level directory is all shared helper functions.  Each solution is ideally a
thin wrapper around the relevant helper functions.
