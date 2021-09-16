[![Build Status](https://travis-ci.com/Copilot-Language/copilot.svg?branch=master)](https://app.travis-ci.com/github/Copilot-Language/copilot)

# Copilot: a stream DSL
The core language, which efficiently represents Copilot expressions.  The core
is only of interest to implementers wishing to add a new back-end to Copilot.

Copilot is a runtime verification framework written in Haskell. It allows the
user to write programs in a simple but powerful way using a stream-based
approach.

Programs can be interpreted for testing, or translated C99 code to be
incorporated in a project, or as a standalone application. The C99 backend
ensures us that the output is constant in memory and time, making it suitable
for systems with hard realtime requirements.


## Installation
Copilot-core can be found on
[Hackage](https://hackage.haskell.org/package/copilot-core). It is typically
only installed as part of the complete Copilot distribution. For installation
instructions, please refer to the [Copilot
website](https://copilot-language.github.io).


## Further information
For further information, install instructions and documentation, please visit
the Copilot website:
[https://copilot-language.github.io](https://copilot-language.github.io)


## License
Copilot is distributed under the BSD-3-Clause license, which can be found
[here](https://raw.githubusercontent.com/Copilot-Language/copilot/master/copilot-core/LICENSE).
