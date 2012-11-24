# Build all the relevant packages in order.

DIRS := copilot-core copilot-language copilot-libraries copilot-sbv copilot-c99 copilot-cbmc Copilot

# Can be overridden with cabal-dev
# use make all CABAL=cabal-dev
CABAL ?= cabal --user --force-reinstalls --reinstall

.PHONY: all $(DIRS)

all: $(DIRS)

$(DIRS): 
	cd ../$@; \
	$(CABAL) install

# Get the repos
.PHONY: get
get:
	git clone https://github.com/leepike/copilot-core.git
	git clone https://github.com/leepike/copilot-c99.git
	git clone https://github.com/leepike/copilot-sbv.git
	git clone https://github.com/leepike/copilot-cbmc.git
	git clone https://github.com/leepike/copilot-language.git
	git clone https://github.com/leepike/copilot-libraries.git
