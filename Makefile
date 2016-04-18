.DEFAULT_GOAL := help

clean: ## Clean Haskell local packages
	@stack clean

install: ## Compile Haskell binary
	@stack install

repl: ## Launch ghci
	@stack ghci match

test: ## Run the tests
	@stack ghci match

watch: ## Compile on file changes
	@stack install --fast --file-watch

help: ## Print documentation
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY:
	clean
	install
	repl
	watch
	help
