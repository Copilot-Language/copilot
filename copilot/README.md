<div align="center">

# Copilot

[![Build Status](https://travis-ci.com/Copilot-Language/copilot.svg?branch=master)](https://app.travis-ci.com/github/Copilot-Language/copilot)
[![Version on Hackage](https://img.shields.io/hackage/v/copilot.svg)](https://hackage.haskell.org/package/copilot)

Copilot is a runtime verification framework for hard real-time systems.
Programs can be interpreted for testing, or translated into C99 code to be
incorporated in a project or standalone application. The C99 code generated
is constant in memory and time, making it suitable for systems with hard
real-time requirements.

[Installation](#installation) •
[Examples](#examples) •
[Related projects](#related-projects) •
[Documentation](#documentation) •
[Contributions](#contributions) •
[Acknowledgements](#acknowledgements)

</div>

## Features

- Write simple, high-level specifications using a stream-based language.

- Produce hard real-time C99 runtime monitors that run in constant memory and time.

- Catch errors in specifications early using expressive static type system.

- Prove properties about specifications using theorem proving extensions.

- Interpret specifications for testing and debugging purposes.

- Obtain proofs of correctness of the generated code.

## Table of Contents

- [Installation](#installation)
  - [Linux installation](#linux-installation)
  - [Mac installation](#mac-installation)
  - [Troubleshooting](#troubleshooting)
- [Examples](#examples)
- [Related projects](#related-projects)
- [Documentation](#documentation)
  - [API documentation and tutorials](#api-documentation-and-tutorials)
  - [Publications](#publications)
  - [Website](#website)
- [Contributions](#contributions)
- [Acknowledgements](#acknowledgements)
  - [The Copilot team](#the-copilot-team)
  - [Institutional support](#institutional-support)

# Installation
<sup>[(Back to top)](#table-of-contents)</sup>

## Linux installation
<sup>[(Back to top)](#table-of-contents)</sup>

### Debian Bookworm / Ubuntu 23.04

On Debian Bookworm / Ubuntu 23.04 or newer, Copilot can be installed directly
from the package repositories with:

```sh
$ sudo apt-get install libghc-copilot-dev
```

To test that Copilot is available, execute the following:
```sh
$ ghci <<< 'import Language.Copilot'
```

It should end with a line like the following and not print any error messages:
```sh
ghci> ghci> Leaving GHCi.
```

### Fedora

On Fedora 40 or newer, Copilot can be installed directly
from the package repositories with:

```sh
$ sudo dnf install ghc-copilot-devel
```

To test that Copilot is available, execute the following:
```sh
$ ghci <<< 'import Language.Copilot'
```

It should end with a line like the following and not print any error messages:
```sh
ghci> ghci> Leaving GHCi.
```

### Other Linux distributions

On other Linux distributions or older Debian-based distributions, to use
Copilot you must install a Haskell compiler (GHC) and the package manager
Cabal. We currently support all versions of GHC from 8.6.5 to modern versions
(9.8 as of this writing). You can install the toolchain using
[ghcup](https://www.haskell.org/ghcup/) or, if you are on Debian/Ubuntu,
directly with `apt-get`:

```sh
$ sudo apt-get install ghc cabal-install
```

Once the compiler is installed, install Copilot from
[Hackage](https://hackage.haskell.org/package/copilot) with:

```sh
cabal v2-install --lib copilot
```

To test that Copilot is available, execute the following:
```sh
$ ghci <<< 'import Language.Copilot'
```

It should end with a line like the following and not print any error messages:
```sh
ghci> ghci> Leaving GHCi.
```

## Mac installation
<sup>[(Back to top)](#table-of-contents)</sup>

To use Copilot you must have a Haskell compiler (GHC) and the package manager
Cabal. We currently support all versions of GHC from 8.6.5 to modern versions
(9.6 as of this writing). You can install the toolchain using
[ghcup](https://www.haskell.org/ghcup/), as well as with Homebrew:

```sh
$ brew install ghc cabal-install
```

Once the compiler is installed, install Copilot from
[Hackage](https://hackage.haskell.org/package/copilot) with:

```sh
$ cabal v2-install --lib copilot
```

To test that Copilot is available, execute the following:
```sh
$ ghci <<< 'import Language.Copilot'
```

It should end with a line like the following and not print any error messages:
```sh
ghci> ghci> Leaving GHCi.
```

## Troubleshooting
<sup>[(Back to top)](#table-of-contents)</sup>

Feel free to open an issue if you are unable to install Copilot following these
instructions.

There is a TravisCI file at the root of the repository that may help with
troubleshooting the installation. Our issues often include comments with
Dockerfiles listing the steps necessary to install Copilot from scratch.

## Examples
<sup>[(Back to top)](#table-of-contents)</sup>

Here follows a simple example of a heating system. More examples can be found
in the [examples
directory](https://github.com/Copilot-Language/copilot/tree/master/copilot/examples)
of the main repository.

```haskell
-- This example implements a simple home heating system. The system heats
-- when the temperature gets too low, and stops when it is high enough. It read
-- temperature as a byte (range -50C to 100C) and translates this to Celsius.

module Heater where

import Language.Copilot
import Copilot.Compile.C99

import Prelude hiding ((>), (<), div)

-- External temperature as a byte, ranging from -50C to 100C.
temp :: Stream Word8
temp = extern "temperature" Nothing

-- Temperature in Celsius.
--
-- We need to cast the Word8 to a Float. This is an unsafeCast, as there
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

If you save this example in a file `Heater.hs` and run:
```sh
$ runhaskell Heater.hs
```
it will produce the files `heater.c`, `heater.h` and `heater_types.h`,
containing, respectively, the implementation of the monitors, the interface,
and a declaration of any types declared in the specification (empty in this
case).

If you clone the repository, the examples in the `examples/` directory can be
run from the root of the project. As a rule of thumb, each example is named
after the filename (without extension) in lowercase letters, and directory
separators replaced with a '-'. For example:

```sh
$ cabal run addmult -f examples
$ cabal run counter -f examples
$ cabal run what4-arithmetic -f examples
```

# Related projects
<sup>[(Back to top)](#table-of-contents)</sup>

_Disclaimer: The following projects are not part of Copilot. Their mention here
does not constitute any form of endorsement._

- [Ogma](https://github.com/nasa/ogma) is a NASA tool to facilitate the
  integration of safe runtime monitors into other systems, including those
  built using NASA's Core Flight System or the Robot Operating System (ROS 2).

- [arduino-copilot](https://hackage.haskell.org/package/arduino-copilot)
  facilitates building copilot applications that run on Arduino.

- [sketch-frp-copilot](https://hackage.haskell.org/package/sketch-frp-copilot)
  extends Copilot with an FRP-like interface.

- [zephyr-copilot](https://hackage.haskell.org/package/zephyr-copilot)
  facilitates building copilot applications that run on boards supported by the
  Zephyr project.

# Documentation
<sup>[(Back to top)](#table-of-contents)</sup>

## API documentation and tutorials
<sup>[(Back to top)](#table-of-contents)</sup>

A tutorial on Copilot can be found
[here](https://copilot-language.github.io/downloads/copilot_tutorial.pdf).

The API is documented throughout the different libraries and published on
Hackage:

- [copilot](https://hackage.haskell.org/package/copilot)
- [copilot-c99](https://hackage.haskell.org/package/copilot-c99)
- [copilot-core](https://hackage.haskell.org/package/copilot-core)
- [copilot-interpreter](https://hackage.haskell.org/package/copilot-interpreter)
- [copilot-language](https://hackage.haskell.org/package/copilot-language)
- [copilot-libraries](https://hackage.haskell.org/package/copilot-libraries)
- [copilot-prettyprinter](https://hackage.haskell.org/package/copilot-prettyprinter)
- [copilot-theorem](https://hackage.haskell.org/package/copilot-theorem)

## Publications
<sup>[(Back to top)](#table-of-contents)</sup>

The best introduction to the fundamentals of Copilot apart from the tutorial
is:

- [Copilot 3](https://ntrs.nasa.gov/citations/20200003164)

Other relevant papers include:

- [Runtime Verification in Real-Time with the Copilot Language: A Tutorial](https://link.springer.com/chapter/10.1007/978-3-031-71177-0_27)

- [Trustworthy Runtime Verification via Bisimulation (Experience Report)](https://dl.acm.org/doi/abs/10.1145/3607841)

- [An Introduction to Copilot](https://copilot-language.github.io/downloads/copilot_tutorial.pdf)

- [Assuring the Guardians](https://link.springer.com/chapter/10.1007/978-3-319-23820-3_6)

- [Experience report: a do-it-yourself high-assurance compiler](https://dl.acm.org/doi/abs/10.1145/2364527.2364553)

- [Compiling an Haskell EDSL to C: A new C back-end for the Copilot runtime verification framework](https://studenttheses.uu.nl/handle/20.500.12932/29176)

- [Challenges in High-Assurance Runtime Verification](https://link.springer.com/chapter/10.1007/978-3-319-47166-2_31)

- [Runtime Monitoring On Hard Real-Time Operating Systems](http://hdl.handle.net/10342/4999)

- [Design and Testing of an Approach to Automated In-Flight Safety Risk Management for sUAS Operations](https://ntrs.nasa.gov/citations/20220005948)

- [The Essence of Reactivity](https://dl.acm.org/doi/10.1145/3609026.3609727)

- [Types that Change: The Extensible Type Design Pattern](https://dl.acm.org/doi/abs/10.1145/3609025.3609475)

- [Automated Translation of Natural Language Requirements to Runtime Monitors](https://link.springer.com/chapter/10.1007/978-3-030-99524-9_21)

- [Copilot: A Hard Real-Time Runtime Monitor](https://link.springer.com/chapter/10.1007/978-3-642-16612-9_26)

- [Copilot: monitoring embedded systems](https://link.springer.com/article/10.1007/s11334-013-0223-x)

- [From Requirements to Autonomous Flight: An Overview of the Monitoring ICAROUS Project](https://arxiv.org/abs/2012.03745)

- [Integrating FRET with Copilot: Automated Translation of Natural Language Requirements to Runtime Monitors](https://ntrs.nasa.gov/citations/20220000049)

- [Monitoring Distributed Real-Time Systems: A Survey and Future Directions](https://ntrs.nasa.gov/citations/20100027427)

- [Monitoring ROS2: from Requirements to Autonomous Robots](https://arxiv.org/abs/2209.14030)

## Website
<sup>[(Back to top)](#table-of-contents)</sup>

For further information, including links to more documentation and the
tutorial, please visit the Copilot website:
[https://copilot-language.github.io](https://copilot-language.github.io).

# Contributions
<sup>[(Back to top)](#table-of-contents)</sup>

We'd love to receive your contributions, be it code fixes, new features, bug
reports, discussions, or anything else that can help the Copilot project.

If you have any comments, questions, ideas, or other topics that you think may
be of interest, start a new discussion
[here](https://github.com/Copilot-Language/copilot/discussions).

If you would like to contribute a fix for an issue, please comment on the issue
indicating that you want to fix it so that we can assign it to you and track
the status on our end. If the issue does not exist, create it first or ask that
an existing discussion be promoted to an issue.

If you are unsure about whether your submission should be filed as an issue or
as a discussion, file it as a discussion. We can always move it later.

To facilitate merging any pull requests that you send, please:
- Reference the issue you are addressing with the text `Refs #<ISSUENUMBER>.`
  at the end of the subject line of each commit message, in *every commit*.
  Replace `<ISSUENUMBER>` with the number of the specific issue that your pull
  request is addressing.
- Describe what each commit does individually *in the commit's message*. It's
  best to err on the side of being more descriptive than less.
- Update the CHANGELOGs in the *last commit(s)*.

You can take a look at the repository's [commit
history](https://github.com/Copilot-Language/copilot/commits/master/) to better
understand the process we follow. Click on each commit to see how we write
commit messages.

# Acknowledgements
<sup>[(Back to top)](#table-of-contents)</sup>

## The Copilot team
<sup>[(Back to top)](#table-of-contents)</sup>

Copilot is currently maintained by:

* Alwyn Goodloe
* Ivan Perez

Past and current team members also include (in alphabetical order):

* Macallan Cruff
* Frank Dedden
* Chris Hathhorn
* Georges-Axel Jolayan
* Jonathan Laurent
* Eli Mendelson
* Robin Morisset
* Sebastian Niller
* Lauren Pick
* Lee Pike
* Will Pogge
* Ryan Spring
* Laura Titolo
* Nis Wegmann

For a complete list of contributors, including external contributors, see:
https://github.com/Copilot-Language/copilot/graphs/contributors

## Institutional support
<sup>[(Back to top)](#table-of-contents)</sup>

We are grateful for NASA Contract NNL08AD13T to Galois, Inc. and the National
Institute of Aerospace, which partially supported this work.

Additionally NASA Langley contracts 80LARC17C0004 and NNL09AA00A supported
further development of Copilot.
