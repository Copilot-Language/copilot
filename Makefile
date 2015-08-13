PACKAGE= \
  copilot-core \
  copilot-theorem \
  copilot-language \
  copilot-libraries \
  copilot-sbv \
  copilot-c99 \
  copilot-cbmc \
  sbv \
  smtlib2

PACKAGEDIR=$(foreach p, $(PACKAGE), lib/$(p)/)

default:
	build

cabal.sandbox.config:
	cabal sandbox init

.PHONY: build
build: cabal.sandbox.config
	cabal sandbox add-source $(PACKAGEDIR)
	cabal install --allow-newer --dependencies-only
	cabal install

# Note: can't do a `cabal run` since there's no cabal file at the top level.

.PHONY: test
test: build
	cabal run copilot-regression
	cabal run copilot-c99-qc

.PHONY: veryclean
veryclean:
	-rm -rf cabal.sandbox.config
	-rm -rf .cabal-sandbox
	-rm -rf dist

