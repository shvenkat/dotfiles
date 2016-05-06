MAKEFLAGS += --warn-undefined-variables
SHELL := /bin/bash -eu -o pipefail -c
# .SHELLFLAGS := -eu -o pipefail -c
# .DEFAULT_GOAL := all
# .DELETE_ON_ERROR:
# .SUFFIXES:

RC := $(HOME)/.rc
LN_FILES := bin/sahup bin/tmuxcolors bin/machine-status
LN_FILES += .bash_profile .certs .colordiffrc
LN_FILES += .gnupg/gpg.conf .gnupg/gpg-agent.conf
LN_FILES += .inputrc .lessfilter .Rprofile .vim .vimrc.less .xinitrc
CP_FILES := .bashrc .gitconfig .tmux.conf .vimrc .zshrc

.PHONY: all
all: $(addprefix $(HOME)/,$(LN_FILES)) $(addprefix $(HOME)/,$(CP_FILES))

$(addprefix $(HOME)/,$(LN_FILES)) : $(HOME)/% : | $(RC)/%
	if [[ $$(basename $*) != $* ]]; then \
	    mkdir -p $$(dirname $@); \
	    chmod 0700 $$(dirname $@); \
	fi
	ln -s $| $@

$(addprefix $(HOME)/,$(CP_FILES)) : $(HOME)/% : | $(RC)/%.stub
	cp $| $@
