PACKAGE= \
  copilot-core \
  copilot-language \
  copilot-libraries \
  copilot-sbv \
  copilot-c99 \
  copilot-cbmc

PACKAGEDIR=$(foreach p, $(PACKAGE), lib/$(p)/)

default:
	build

cabal.sandbox.config:
	cabal sandbox init

.PHONY: build
build: cabal.sandbox.config
	cabal sandbox add-source $(PACKAGEDIR)
	cabal install $(PACKAGEDIR)

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

################################################################################
# Uploading to Hackage

DISSTR=dist-
DIST := $(patsubst %, $(DISSTR)%, $(ALLDIR))
UPSTR=upload-
UPLOAD := $(patsubst %, $(UPSTR)%, $(ALLDIR))

.PHONY: dist
dist: $(DIST)

.PHONY: $(DIST)
$(DIST):
	cd $(subst $(DISSTR),../, $@) && cabal sdist && cabal check

.PHONY: upload
upload: $(UPLOAD)

.PHONY: $(UPLOAD)
$(UPLOAD):
	cd $(subst $(UPSTR),../, $@) && cabal upload dist/*.gz
