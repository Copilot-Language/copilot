# Build all the relevant packages in order.

DIRS := copilot-core copilot-language copilot-libraries copilot-sbv copilot-c99 copilot-cbmc Copilot

# We recommend using cabal-dev.  But you can use cabal, if you wish.  Just redefine the variable.
CABALDEV=cabal-dev install

.PHONY: all $(DIRS)

all: $(DIRS)

$(DIRS):
	$(CABALDEV) ../copilot-core ../copilot-c99 ../copilot-sbv ../copilot-cbmc ../copilot-language ../copilot-libraries ./

# Get the repos
.PHONY: get
get:
	git clone https://github.com/leepike/copilot-core.git ../copilot-core
	git clone https://github.com/leepike/copilot-c99.git ../copilot-c99
	git clone https://github.com/leepike/copilot-sbv.git ../copilot-sbv
	git clone https://github.com/leepike/copilot-cbmc.git ../copilot-cbmc
	git clone https://github.com/leepike/copilot-language.git ../copilot-language
	git clone https://github.com/leepike/copilot-libraries.git ../copilot-libraries
