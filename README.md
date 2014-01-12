## Install

**Note for OS X 10.9 Maverics:** you must follow
[these directions](http://justtesting.org/post/64947952690/the-glasgow-haskell-compiler-ghc-on-os-x-10-9)
before continuing!

Download the [Haskell Platform 2012.2.0.0 or later](http://hackage.haskell.org/platform/).
Once the Haskell Platform is installed:

    cabal update
    cabal install noelm
    cabal install noelm-server

## Use

To use `noelm` and `noelm-server` you may need to add a new directory to your PATH.

Cabal should tell you where your executables are located upon
successful installation.

For me, the executables were placed in `/home/timothy/.cabal/bin` which I
appended to the end of my PATH variable in my .bashrc file.

See this tutorial if you are new to changing your PATH in
[Unix/Linux](http://www.cyberciti.biz/faq/unix-linux-adding-path/).

#### Final Notes

The `noelm` package provides support for compilation of Noelm code directly in Haskell.
Check it out [on Hackage](http://hackage.haskell.org/package/Noelm) if you are interested.
