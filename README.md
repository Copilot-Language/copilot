[![Build Status](https://travis-ci.org/Copilot-Language/copilot-core.svg?branch=master)](https://travis-ci.org/Copilot-Language/copilot-core)

Overview
========
[copilot-core](http://hackage.haskell.org/package/copilot-core) The core
language, which efficiently represents Copilot expressions.  The core is only of
interest to implementers wishing to add a new back-end to Copilot.

Copilot is a stream (i.e., infinite lists) domain-specific language (DSL) in
Haskell that compiles into embedded C.  Copilot is similar in spirit to
languages like Lustre.  Copilot contains an interpreter, multiple back-end
compilers, and other verification tools.

Installation
============
The Copilot library is cabalized. Assuming you have cabal and the GHC compiler
installed (the [Haskell Platform](http://hackage.haskell.org/platform/) is the
easiest way to obtain these), it should merely be a matter of running 
     
         cabal install copilot-core

However, we strongly recommend you install Copilot, which installs copilot-c99
and other packages automatically.  Execute

         cabal install copilot

Resources
=========
To understand our use of Dynamic typing, please consult the following,
particularly the last document:

+ Carette, Jacques and Kiselyov, Oleg and Shan, Chung-chieh,
  "*Finally tagless, partially evaluated: Tagless staged interpreters for simpler typed languages*",
  Journal of Functional Programming vol. 19, p. 509-543, 2009.

+ Guillemette, Louis-Julien and Monnier, Stefan,
  "*Type-Safe Code Transformations in Haskell*",
  Electronic Notes in Theoretical Computer Science vol. 174, p. 23-39, 2007.

+ Baars, Arthur I. and Swierstra, S. Doaitse,
  "*Typing dynamic typing*",
  ACM SIGPLAN Notices vol. 37, p. 157-166, 2002

Resources
=========
[copilot-core](http://hackage.haskell.org/package/copilot-core) is available on
Hackage.

**Sources** for each package are available on Github as well.  Just go to
[Github](github.com) and search for the package of interest.  Feel free to fork!

Copyright, License
==================
Copilot is distributed with the BSD3 license. The license file contains the
[BSD3](http://en.wikipedia.org/wiki/BSD_licenses) verbiage.

Thanks
======
We are grateful for NASA Contract NNL08AD13T to [Galois,
Inc](http://corp.galois.com/) and the [National Institute of
Aerospace](http://www.nianet.org/), which partially supported this work.
