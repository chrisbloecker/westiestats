build:
	@stack install --local-bin-path .

release:
	@make build
	@docker build -t westiestats:latest .

dev:
	@make build
	@docker build -t westiestats:dev .

.PHONY: build release dev
