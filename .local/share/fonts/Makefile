MAKEFLAGS += --warn-undefined-variables
SHELL := /bin/bash -eu -o pipefail -c
# .SHELLFLAGS := -eu -o pipefail -c
.DEFAULT_GOAL := all
.DELETE_ON_ERROR:
# .SUFFIXES:

FONT_PATCHER := $(HOME)/side/fontpatcher/scripts/powerline-fontpatcher
FONT_PATCH := powerline/powerline-tmux-symbols.sfd
DROID_BASE := droid/DroidSansMono-SlashedZero

all: droid
.PHONY: droid
droid: $(DROID_BASE)-PowerlineTmux.ttf

$(DROID_BASE)-PowerlineTmux.ttf: $(DROID_BASE).ttf $(FONT_PATCH)
	$(FONT_PATCHER) --source-font $(FONT_PATCH) $<
