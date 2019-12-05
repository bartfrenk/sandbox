.DEFAULT_GOAL := help

help: ## Show this help
	@echo
	@fgrep -h "##" $(MAKEFILE_LIST) | \
	fgrep -v fgrep | sed -e 's/## */##/' | column -t -s##
	@echo

book: ## Show the Rust book
	rustup docs --book

