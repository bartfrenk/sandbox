PREFIX := experiment
APP_NAME := python-asyncio

help: ## Show this help
	@echo "${PREFIX}/${APP_NAME}"
	@echo
	@fgrep -h "##" $(MAKEFILE_LIST) | \
	fgrep -v fgrep | sed -e 's/## */##/' | column -t -s##

redis: ## Start Redis instance
	docker run --detach\
		--name ${PREFIX}-${APP_NAME}-redis \
		--rm --publish "16379:6379" redis

export PYTHONPATH=./src:./test

app-consumers: ## Run Huey consumers
	huey_consumer.py app.interactions.huey -k process -w 4
