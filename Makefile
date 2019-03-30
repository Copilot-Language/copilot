default: build

PACKAGEDIRS=$(shell find lib/ -iname '*.cabal'  -exec dirname {} \;)

cabal.sandbox.config:
	cabal sandbox init

.PHONY: build
build: cabal.sandbox.config
	cabal sandbox add-source ${PACKAGEDIRS}
	cabal install  --dependencies-only --force-reinstalls
	cabal install

# Note: can't do a `cabal run` since there's no cabal file at the top level.

.PHONY: test
test: build
	#cabal run copilot-regression
	#cabal run copilot-c99-qc


.PHONY: veryclean
veryclean:
	-rm -rf cabal.sandbox.config
	-rm -rf .cabal-sandbox
	-rm -rf dist


include Examples/examples.mk
