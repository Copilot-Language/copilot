# Build all the relevant packages in order.

DIRS := copilot-core copilot-language copilot-libraries copilot-sbv copilot-c99 copilot-cbmc Copilot

.PHONY: all $(DIRS)

all: $(DIRS)

$(DIRS): 
	cd ../$@; \
	cabal install

