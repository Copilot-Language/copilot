[![Build Status](https://travis-ci.com/Copilot-Language/copilot.svg?branch=master)](https://app.travis-ci.com/github/Copilot-Language/copilot)

# Copilot: a stream DSL
User-supplied libraries for Copilot, including linear-temporal logic,
fault-tolerant voting, regular expressions, etc.

Copilot is a runtime verification framework written in Haskell. It allows the
user to write programs in a simple but powerful way using a stream-based
approach.

Programs can be interpreted for testing, or translated C99 code to be
incorporated in a project, or as a standalone application. The C99 backend
ensures us that the output is constant in memory and time, making it suitable
for systems with hard realtime requirements.


## Installation
Copilot-libraries can be found on
[Hackage](https://hackage.haskell.org/package/copilot-libraries). It is typically
only installed as part of the complete Copilot distribution. For installation
instructions, please refer to the [Copilot
website](https://copilot-language.github.io).


## Further information
For further information, install instructions and documentation, please visit
the Copilot website:
[https://copilot-language.github.io](https://copilot-language.github.io)


## License
Copilot is distributed under the BSD-3-Clause license, which can be found
[here](https://raw.githubusercontent.com/Copilot-Language/copilot/master/copilot-libraries/LICENSE).
