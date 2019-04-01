default: build

PACKAGEDIRS=$(shell find lib/ -iname '*.cabal'  -exec dirname {} \;)

cabal.sandbox.config:
	cabal sandbox init

.PHONY: build
build: cabal.sandbox.config
	cabal sandbox add-source ${PACKAGEDIRS}
	cabal install  --dependencies-only --force-reinstalls
	cabal install

.PHONY: veryclean
veryclean:
	-rm -rf cabal.sandbox.config
	-rm -rf .cabal-sandbox
	-rm -rf dist
