[![Build Status](https://travis-ci.org/Copilot-Language/Copilot.svg?branch=master)](https://travis-ci.org/Copilot-Language/Copilot)

# Copilot: a stream DSL
Copilot is a runtime verification framework written in Haskell. It allows the
user to write programs in a simple but powerful way using a stream-based
approach.

Programs can be interpreted for testing, or translated C99 code to be
incorporated in a project, or as a standalone application. The C99 backend
ensures us that the output is constant in memory and time, making it suitable
for systems with hard realtime requirements.


## Installation
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
      git submodule update --init --remote
      make

Note there is a TravisCI build (linked to at the top of this README) if you
have trouble building/installing.


## Contributions
Feel free to open new issues and send pull requests.

In order to contribute to Copilot, please use the following steps which will
make the process of evaluating and including your changes much easier:

* Create an issue for every individual change or problem with Copilot. Document
  the issue well.

* Always comment on the issues you are addressing in every commit. Be
  descriptive, and use the syntax `#<issue_number>` so that we can track
  changes and issues easily.

* Every commit should mention one issue and, ideally, only one.

* Do not send a PR or commit that addresses multiple problems, unless they are
  related and cannot be separated.

* Do not commit to master directly, except for branch merges. Make sure you
  always merge onto master using `--no-ff` so that we can tell that features
  were addressed separately, completed, tested, and then merged.  If you are a
  Copilot developer, create a branch for every issue you are addressing, complete
  it, and then merge onto master. Document every commit in every branch,
  including the last merge commit, stating the issues it addresses or closes.

This process is similar to [Git
Flow](http://nvie.com/posts/a-successful-git-branching-model/). The equivalent
of Git Flow's master branch is our latest tag, and the equivalent of Git Flow's
develop branch is our master.


## Further information
For further information, including documentation and a tutorial, please visit
the Copilot website:
[https://copilot-language.github.io](https://copilot-language.github.io)


## Acknowledgements
We are grateful for NASA Contract NNL08AD13T to Galois, Inc. and the National
Institute of Aerospace, which partially supported this work.


## License
Copilot is distributed under the BSD-3-Clause license, which can be found
[here](https://raw.githubusercontent.com/Copilot-Language/Copilot/master/LICENSE)


## The Copilot Team
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
