MAKEFLAGS += --warn-undefined-variables
SHELL := /bin/bash -eu -o pipefail -c

it: install

so: config

install: config brew
	brew install --HEAD bork
	bork/bootstrap.sh

config: git
	fresh/bootstrap.sh

brew:

.PHONY: it so install config brew
