[![Build Status](https://travis-ci.org/Copilot-Language/Copilot.svg?branch=master)](https://travis-ci.org/Copilot-Language/Copilot)

# Copilot: a stream DSL
Copilot is a runtime verification framework written in Haskell. It allows the
user to write programs in a simple but powerful way using a stream-based
approach.

Programs can be interpreted for testing, or translated C99 code to be
incorporated in a project, or as a standalone application. The C99 backend
ensures us that the output is constant in memory and time, making it suitable
for systems with hard realtime requirements.


# Installation
There are two ways to install Copilot:

* From Hackage (Recommended way):

  The Copilot library is cabalized. Assuming you have cabal and the GHC
  compiler installed (the
  [Haskell Platform](http://hackage.haskell.org/platform/) is the easiest way
  to obtain these), it should merely be a matter of running

      cabal install copilot

  with an Internet connection.

* Building from source from the GitHub repositories, typically one would only
  go this route to develop Copilot.

      git clone https://github.com/Copilot-Language/Copilot.git
      cd Copilot
      git submodule update --remote
      make

Note there is a TravisCI build (linked to at the top of this README) if you
have trouble building/installing.


# Further information
For further information, including documentation and a tutorial, please visit
the Copilot website:
[https://copilot-language.github.io](https://copilot-language.github.io)


# Acknowledgements
We are grateful for NASA Contract NNL08AD13T to Galois, Inc. and the National
Institute of Aerospace, which partially supported this work.


# License
Copilot is distributed under the BSD-3-Clause license, which be found [here](
https://raw.githubusercontent.com/Copilot-Language/Copilot/master/LICENSE)


# The Copilot Team
The development of Copilot spans across several years. During these years
the following people have helped develop Copilot (in no particular order):

* Lee Pike
* Alwyn Goodloe (maintainer)
* Robin Morisset
* Levent Erkők
* Sebastian Niller
* Nis Wegmann
* Chris Hathhorn
* Eli Mendelson
* Jonathan Laurent
* Laura Titolo
* Georges-Axel Jolayan
* Macallan Cruff
* Ryan Spring
* Lauren Pick
* Frank Dedden (maintainer: contact at dev@dedden.net)
* Ivan Perez
