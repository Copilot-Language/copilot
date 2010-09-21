#!/bin/bash
# Make documentation using Haddock and move to doc/

cabal haddock --haddock-option=--source-base=http://github.com/leepike/Copilot --haddock-option=--source-module=http://github.com/leepike/Copilot/%M.html
cp dist/doc/html/copilot/* doc/
