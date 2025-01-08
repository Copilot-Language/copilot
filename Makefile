all: help-banner help

PHONY: help
help: ## Show the commented targets.
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
	sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

PHONY: help-banner
help-banner: ## Show the help banner.
	@echo "===================================================================="
	@echo "§ all                  make with no arguments also shows this banner"
	@echo "§ help                 make help will list targets with descriptions"
	@echo "===================================================================="

.PHONY: typos-install
typos-install: ## Install typos-cli for typos target using cargo.
	cargo install typos-cli

GREP_EXCLUDE := grep -v -E 'dist-'
FIND_NAMED := find . -type f -name

.PHONY: markdown-typos
markdown-typos: ## Find typos in Markdown .md files.
	$(FIND_NAMED) '*.md' | $(GREP_EXCLUDE) | xargs typos

.PHONY: markdown-fix-typos
markdown-fix-typos: ## Fix typos in Markdown .md files.
	$(FIND_NAMED) '*.md' | $(GREP_EXCLUDE) | xargs typos --write-changes

.PHONY: hs-typos
hs-typos: ## Find typos in Haskell .hs files.
	$(FIND_NAMED) '*.hs' | $(GREP_EXCLUDE) | xargs typos

.PHONY: hs-fix-typos
hs-fix-typos: ## Fix typos in Haskell .hs files.
	$(FIND_NAMED) '*.hs' | $(GREP_EXCLUDE) | xargs typos --write-changes
