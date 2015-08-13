PACKAGE= \
  copilot-core \
  copilot-theorem \
  copilot-language \
  copilot-libraries \
  copilot-sbv \
  copilot-c99 \
  copilot-cbmc \
  sbv

PACKAGEDIR=$(foreach p, $(PACKAGE), lib/$(p)/)

default:
	build

cabal.sandbox.config:
	cabal sandbox init

.PHONY: build
build: cabal.sandbox.config
	git clone --depth 1 https://github.com/hguenther/smtlib2 lib/smtlib2
	cd lib/smtlib2 && cabal install --allow-newer --only-dependencies && cabal configure && cabal build && cabal install
	cd ../..
	git clone --depth 1 https://github.com/LeventErkok/sbv lib/sbv
	cabal sandbox add-source $(PACKAGEDIR)
	cabal install --dependencies-only

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

