MAKEFLAGS += --warn-undefined-variables
SHELL := /bin/bash -eu -o pipefail -c
OSTYPE := $(shell echo $$OSTYPE)

RC := $(HOME)/.rc
LN_FILES := bin/sahup bin/machine-dashboard bin/machine-status
LN_FILES += .bash_profile .certs .colordiffrc .editorconfig
LN_FILES += .gnupg/gpg.conf .gnupg/gpg-agent.conf
LN_FILES += .inputrc .lessfilter .Rprofile .vim .vimrc.less .xinitrc
CP_FILES := .bashrc .gitconfig .tmux.conf .vimrc

.PHONY: all
all: $(HOME)/.zshrc
all: $(HOME)/.config/nvim/init.vim
all: $(addprefix $(HOME)/,$(LN_FILES)) $(addprefix $(HOME)/,$(CP_FILES))
all: $(HOME)/bin/diff-highlight
all: $(HOME)/.htoprc

$(HOME)/.zshrc : $(RC)/.zshrc.stub
	# Install zsh plugins.
	git clone https://github.com/zplug/zplug ~/.zplug
	cp $< $@

$(HOME)/.config/nvim/init.vim : $(RC)/nvim/init.vim
	mkdir -p ~/.config/nvim/{autoload,after}
	curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs \
	    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
	ln -s $< $@
	ln -s -t $(HOME)/.config/nvim/ $(RC)/nvim/after
	ln -s -t $(HOME)/.config/nvim/autoload/ $(RC)/nvim/autoload/*
	ln -s -t $(HOME)/.config/nvim/ultisnips $(RC)/nvim/ultisnips

$(addprefix $(HOME)/,$(LN_FILES)) : $(HOME)/% : | $(RC)/%
	if [[ $$(basename $*) != $* ]]; then \
	    mkdir -p $$(dirname $@); \
	    chmod 0700 $$(dirname $@); \
	fi
	ln -s $| $@

$(addprefix $(HOME)/,$(CP_FILES)) : $(HOME)/% : | $(RC)/%.stub
	cp $| $@

$(HOME)/.htoprc: $(RC)/.htoprc
	cp $< $@

DIFF_HIGHLIGHT:=$(if $(findstring linux,$(OSTYPE)),/usr/share/doc/git/contrib/diff-highlight/diff-highlight,)
ifneq (,$(findstring linux,$(OSTYPE)))
$(HOME)/bin/diff-highlight: $(DIFF_HIGHLIGHT)
	cp $(DIFF_HIGHLIGHT) $@
	chmod +x $@
else
output != test -x $(HOME)/bin/diff-highlight
ifneq ($(.SHELLSTATUS),0)
$(HOME)/bin/diff-highlight:
	echo "Please install $(HOME)/bin/diff-highlight"
endif
endif
