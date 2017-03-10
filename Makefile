MAKEFLAGS += --warn-undefined-variables
SHELL := /bin/bash -eu -o pipefail -c

all: it so

help:
	echo "To install config and apps, type: make it so."

it: install

so: config

install: config brew
	brew install --HEAD bork
	bork/bootstrap.sh
	echo "And... Engage!"

config: git
	fresh/bootstrap.sh

brew:

.PHONY: all help it so install config brew
