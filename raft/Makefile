.PHONY: jar clean migrations run test build publish push deploy run-as-docker
.DEFAULT_GOAL := help

NAME := raft

export ENV ?= dev

export CONFIG=env/${ENV}.edn

help: ## Show this help
	@echo "${NAME}"
	@echo
	@fgrep -h "##" $(MAKEFILE_LIST) | \
	fgrep -v fgrep | sed -e 's/## */##/' | column -t -s##

clean: ## Remove build artifacts
clean:
	rm -f app.jar
	rm -f VERSION
	rm -rf target

run: ## Run the service
	clj -A:app

test: ## Run tests
	clj -A:test

version:
	git describe --tags --always --dirty > VERSION

build: ## Build the image
build: version
	docker build --tag ${CI_REGISTRY_IMAGE}:$(shell cat VERSION) .
