# Copilot: Stream DSL for hard real-time runtime verification

[![Build Status](https://travis-ci.org/Copilot-Language/Copilot.svg?branch=master)](https://travis-ci.org/Copilot-Language/Copilot)
[![Version on Hackage](https://img.shields.io/hackage/v/copilot.svg)](https://hackage.haskell.org/package/copilot)

Copilot is a runtime verification framework written in Haskell. It allows the
user to write programs in a simple but powerful way using a stream-based
approach.

Programs can be interpreted for testing, or translated into C99 code to be
incorporated in a project, or as a standalone application. The C99 backend
output is constant in memory and time, making it suitable for systems with hard
realtime requirements.


## Using Copilot
Assuming you have GHC and cabal already installed (see [Haskell
Platform](http://hackage.haskell.org/platform/) or
[ghcup](https://www.haskell.org/ghcup/)), there are several ways to use
Copilot:

* Adding Copilot to your project

  Copilot is available from
  [Hackage](https://hackage.haskell.org/package/copilot). Adding `copilot`
  to your project's cabal file should be enough to get going.

* Adding Copilot to the default GHC environment

   ```bash
   cabal v2-install --lib copilot
   ```

  After which Copilot will be available from ghci.

* Launching a repl with Copilot

  Another quick solution is to cabal to launch a repl with Copilot
  available.

  ```bash
  cabal v2-repl --build-depends copilot
  ```

  Cabal will download and build Copilot only to make it available in the
  launched repl. The global GHC environment will not be affected.

* Building from source (typically done for development):

   ```bash
   git clone https://github.com/Copilot-Language/Copilot.git
   cd Copilot
   git submodule update --init --remote
   cabal v2-build
   ```

Note there is a TravisCI build (linked to at the top of this README) if you
have trouble building/installing.


## Example
Here follows a simple example of a heating system. Other examples can be found
in the [Examples
directory](https://github.com/Copilot-Language/Copilot/tree/master/Examples)
of the main repository.

```haskell
-- This is a simple example with basic usage. It implements a simple home
-- heating system: It heats when temp gets too low, and stops when it is high
-- enough. It read temperature as a byte (range -50C to 100C) and translates
-- this to Celcius.

module Heater where

import Language.Copilot
import Copilot.Compile.C99

import Prelude hiding ((>), (<), div)

-- External temperature as a byte, range of -50C to 100C
temp :: Stream Word8
temp = extern "temperature" Nothing

-- Calculate temperature in Celcius.
-- We need to cast the Word8 to a Float. Note that it is an unsafeCast, as there
-- is no direct relation between Word8 and Float.
ctemp :: Stream Float
ctemp = (unsafeCast temp) * (150.0 / 255.0) - 50.0

spec = do
  -- Triggers that fire when the ctemp is too low or too high,
  -- pass the current ctemp as an argument.
  trigger "heaton"  (ctemp < 18.0) [arg ctemp]
  trigger "heatoff" (ctemp > 21.0) [arg ctemp]

-- Compile the spec
main = reify spec >>= compile "heater"
```

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
[https://copilot-language.github.io](https://copilot-language.github.io).


## Acknowledgements
We are grateful for NASA Contract NNL08AD13T to Galois, Inc. and the National
Institute of Aerospace, which partially supported this work.

Additionally NASA Langley contracts 80LARC17C0004 and NNL09AA00A supported
further development of Copilot.


## License
Copilot is distributed under the BSD-3-Clause license, which can be found
[here](https://raw.githubusercontent.com/Copilot-Language/Copilot/master/LICENSE).


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
