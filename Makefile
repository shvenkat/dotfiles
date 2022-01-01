# IAQ
# ---
#
# Q. "How do I use this Makefile?"
# A. You don't. I do. Write your own damn Makefile :)
#
#    But if you insist, so be it. You first run `make help` to view all
#    available targets. Then `make <target>` to install one or more programs
#    and/or their configuration. You can use `make -s <target>` for less verbose
#    output.
#
# Q. Why do you have so many targets in this fakakta Makefile?
# A. I use this Makefile to manage the programs I often use and their
#    configuration. There are quite a few of these and I needed a way to set
#    them up (juuuust the way I like them) very quickly.
#
# Q. Why use a Makefile just to copy/symlink your dotfiles?
# A. This Makefile does more than copy or symlink my dotfiles into my home
#    directory. It can also install programs and their extensions, update these
#    and remove them. Each of these actions for any given program is performed
#    by a few lines of shell script, but can depend on other actions. make is
#    well suited to expressing dependencies between shell snippets, especially
#    when the snippets create or update files, as is the case here.
#
#    Previously, I had a set of shell scripts handle installation of software
#    prerequisites, programs and configuration. Over time, it grew into a
#    monolith, such that I couldn't quickly install, update and configure
#    individual programs, which is handy when performing a quick task on a
#    remote machine. This need arose far more often than a complete installation
#    of all programs and configuration. In addition, the shell scripts were
#    scattered across this repo and sometimes distracting to locate and run.
#    With 'make', I get the fine granularity and unified interface I need.
#
# Q. What targets are available and what do they do?
# A. There are a large number of targets that can be listed with `make help`.
#    These can be grouped by program (e.g. emacs, zsh) or category (e.g. prog,
#    config, update, clean). In general, a program named 'foo' may have the
#    targets listed below. Very few programs will have all.
#
#      * foo: a wrapper around foo-{prog,config}.
#      * foo-prog: installs program foo using an OS-dependent package manager.
#      * foo-config: installs/updates configuration for program foo.
#      * foo-update: updates program foo and its configuration.
#
#      * foo-clean: a wrapper around foo-{prog,config}-clean.
#      * foo-prog-clean: uninstall foo.
#      * foo-config-clean: remove configuration for program foo.
#
#    In addition, there are 'top-level' wrapper targets for convenience. For
#    instance, 'progs' wraps all '*-prog' targets, and 'configs' wraps all
#    '*-config' targets. Again, see `make-help` for a full list.
#
# Q. What is the dependency logic between targets?
# A. For a program foo, the target foo-prog can depend on foo-config. Thus,
#    configuration can be installed very quickly as it doesn't require program
#    or extension installation. And, the foo-prog also leaves the program
#    configured.
#
# Q. Are there any software requirements for using this Makefile?
# A. A *nix environment, make and bash are assumed to be present. Other basic
#    software requirements for this Makefile - such as curl and git - are kept
#    to a minimum and declared as prerequisites for the relevant targets. Their
#    installation _may_ be left to the user, in which case a message may be
#    printed asking the user to install them.
#
# Q. How are differences between OSes handled?
# A. `uname -s` is used to identify the kernel, and `/etc/*release` the Linux
#    distribution if needed. Parts of this Makefile are conditioned on these
#    results. Prior to installation of OS-specific programs, like iTerm2, the
#    presence of the matching OS is checked. For installation of cross-platform
#    programs, like emacs, the OS package manager is used.


.DEFAULT_GOAL:=help
MAKEFLAGS+=--warn-undefined-variables
SHELL:=/bin/bash -e -o pipefail -u -c
.DELETE_ON_ERROR:

OSTYPE := $(shell uname -s | tr '[:upper:]' '[:lower:]')
ifeq ($(OSTYPE),linux)
# See https://unix.stackexchange.com/a/6348.
DISTRO := $(shell source /etc/os-release && echo "$$NAME")
# PKG_MANAGER :=
# PKG_INSTALL :=
# PKG_CHECK :=
# PKG_UPDATE :=
# PKG_REMOVE :=
else ifeq ($(OSTYPE),darwin)
else
$(error "Unknown OS: $(OSTYPE)")
endif


#  ----------  PREREQ. TARGETS  ------------------------------------------------

.PHONY: prereq buildtools curl git bash5
.SILENT: prereq buildtools curl git

prereq: buildtools curl git

ifeq ($(OSTYPE),linux)

buildtools:
	{ echo "build tools build-essential/file not found" 1>&2; exit 1; }

curl:
	which curl &>/dev/null || { echo "curl not found" 1>&2; exit 1; }

git:
	which git &>/dev/null || { echo "git not found" 1>&2; exit 1; }

python3:
	which python3 &>/dev/null || { echo "python3 not found" 1>&2; exit 1; }

gpg:
	which gpg &>/dev/null || { echo "gpg not found" 1>&2; exit 1; }

bash5: $(HOME)/bin/bash5 curl gpg
$(HOME)/bin/bash5: | curl gpg
	build_dir="$$(mktemp --directory)" ;\
	cd "$$build_dir" ;\
	curl -fsS \
	    -O http://ftp.gnu.org/gnu/bash/bash-5.0.tar.gz \
	    -O http://ftp.gnu.org/gnu/bash/bash-5.0.tar.gz.sig ;\
	gpg --verify bash-5.0.tar.gz.sig bash-5.0.tar.gz ;\
	tar -xzf bash-5.0.tar.gz ;\
	for i in $$(seq --equal-width 1 999); do \
	    curl -fsS \
	        -O "http://ftp.gnu.org/gnu/bash/bash-5.0-patches/bash50-$${i}" \
	        -O "http://ftp.gnu.org/gnu/bash/bash-5.0-patches/bash50-$${i}.sig" \
	    || break ;\
	    gpg --verify "bash50-$${i}.sig" "bash50-$${i}" ;\
	    patch --directory=bash-5.0 -p0 < "bash50-$${i}" ;\
	done ;\
	cd bash-5.0 ;\
	./configure ;\
	make ;\
	cp bash $@ ;\
	cd ;\
	&& rm -rf "$$build_dir"

else ifeq ($(OSTYPE),darwin)

buildtools:
	which clang &>/dev/null || xcode-select --install

curl git ruby:
	if ! which $@ &>/dev/null; then \
	    echo -e '\033[31mERROR: $@ not found.\033[0m' \
	        'Please install and/or update PATH.'; \
	    exit 1; \
	fi

.PHONY: brew python3 ruby
.SILENT: brew python3 ruby

prereq: brew python3

brew: curl ruby git buildtools
	if ! test -x /usr/local/bin/brew; then \
	    url='https://raw.githubusercontent.com/Homebrew/install/master/install'; \
	    if ! install_rb="$$(curl -fsS "$$url")"; then \
	        echo -e '\033[31mERROR downloading Homebrew installer\033[0m' \
	            "from $$url, got:\n$$install_rb"; \
	        exit 1; \
	    fi; \
	    ruby -e "$$install_rb"; \
	fi
	if ! which brew &>/dev/null; then \
	    echo "Error: brew not found. Please add /usr/local/bin to PATH."; \
	    exit 1; \
	fi

python3: brew
	if ! which python3 &>/dev/null; then \
	    brew install python3; \
	fi

bash5:
	brew install bash

endif


#  ----------  DOTFILE TARGETS  ------------------------------------------------

DOTFILES := \
        bash-config \
        readline-config \
        ssh-config \
        gnupg-config \
        emacs-config \
        editorconfig-config \
        git-config \
        tmux-config \
        colordiff-config \
        python-config \
        jupyter-config \
        less-config \
        htop-config \
        unison-config \
        x-config \
        firejail-config \
        firefox-config \
        font-config
        # xfce4-terminal-config \
        # lxterminal-config
        # zsh-config \

.PHONY: dotfiles $(DOTFILES)

dotfiles: $(DOTFILES)

emacs-config: $(HOME)/.emacs.d/init.el
$(HOME)/.emacs.d/init.el: \
        $(addprefix emacs/, \
            usepackage_quelpa.el \
            evil.el \
            whitespace.el \
            pairs.el \
            wrap.el \
            margins.el \
            menu_and_mode_bars.el \
            theme.el \
            fonts.el \
            splash_message.el \
            backup.el \
            misc_keys.el \
            mouse.el \
            ido.el \
            editorconfig.el \
            flycheck.el \
            org.el \
            markdown.el \
            python.el \
            bash.el \
            racket.el \
            custom.el)

xfce4-terminal: $(HOME)/.config/xfce4/terminal/terminalrc
$(HOME)/.config/xfce4/terminal/terminalrc: linux/xfce4_terminalrc

lxterminal: $(HOME)/.config/lxterminal/lxterminal.conf
$(HOME)/.config/lxterminal/lxterminal.conf: linux/lxterminal.conf

zsh-config: $(addprefix $(HOME)/,.zprofile .zshrc .zplug .autoenv.zsh)
$(HOME)/.zprofile: $(addprefix shell/shared/,path env term)
$(HOME)/.zshrc: \
        $(addprefix shell/shared/,misc less ls source-highlight ssh alias) \
        shell/zsh/zshrc \
        shell/shared/local
$(HOME)/.autoenv.zsh: shell/zsh/autoenv.zsh

bash-config: $(addprefix $(HOME)/,.bash_profile .bashrc)
$(HOME)/.bash_profile: $(addprefix shell/shared/,path env term) \
        shell/bash/bash_profile
$(HOME)/.bashrc: \
        shell/bash/bashrc \
        $(addprefix shell/shared/,misc less ls source-highlight ssh alias) \
        shell/shared/local

git-config: \
        $(addprefix $(HOME)/.config/git/,config ignore attributes) \
        $(HOME)/bin/diff-highlight
$(HOME)/.config/git/config: git/config
$(HOME)/.config/git/ignore: git/ignore
$(HOME)/.config/git/attributes: git/attributes

tmux-config: $(HOME)/.tmux.conf
$(HOME)/.tmux.conf: tmux/tmux.conf tmux/colorscheme_solarized_light.conf

ssh-config: $(HOME)/bin/ssh-agent-connect
$(HOME)/bin/ssh-agent-connect: bin/ssh-agent-connect

gnupg-config: $(addprefix $(HOME)/.gnupg/,gpg.conf gpg-agent.conf dirmngr.conf)
$(HOME)/.gnupg/gpg.conf: gnupg/gpg.conf gnupg-dir
$(HOME)/.gnupg/gpg-agent.conf: gnupg/gpg-agent.conf gnupg-dir
$(HOME)/.gnupg/dirmngr.conf: gnupg/dirmngr.conf gnupg-dir
.PHONY: gnupg-dir
gnupg-dir:
	mkdir -p $(HOME)/.gnupg && chmod 0700 $(HOME)/.gnupg

readline-config: $(HOME)/.inputrc
$(HOME)/.inputrc: util/inputrc

colordiff-config: $(HOME)/.colordiffrc
$(HOME)/.colordiffrc: util/colordiffrc

htop-config: $(HOME)/.config/htop/htoprc
$(HOME)/.config/htop/htoprc: util/htoprc

less-config: $(HOME)/bin/lesspipe source-highlight-config
$(HOME)/bin/lesspipe: bin/lesspipe

.PHONY: source-highlight-config
source-highlight-config: \
        $(HOME)/.config/source-highlight/esc.style \
        $(HOME)/.local/share/source-highlight
$(HOME)/.config/source-highlight/esc.style: util/source-highlight.style
$(HOME)/.local/share/source-highlight: source-highlight

unison-config: $(HOME)/.unison/default.prf
$(HOME)/.unison/default.prf: util/unison_default.prf

editorconfig-config: $(HOME)/.editorconfig
$(HOME)/.editorconfig: util/editorconfig

python-config: $(addprefix $(HOME)/,.mypy.ini .flake8 .pylintrc .isort.cfg)
$(HOME)/.mypy.ini: python/mypy.ini
$(HOME)/.flake8: python/flake8
$(HOME)/.pylintrc: python/pylintrc
$(HOME)/.isort.cfg: python/isort.cfg

jupyter-config: $(HOME)/.jupyter/jupyter_notebook_config.py
jupyter-config: $(addprefix $(HOME)/.jupyter/lab/user-settings/@jupyterlab/, \
		application-extension \
		apputils-extension \
		codemirror-extension \
		docmanager-extension \
		extensionmanager-extension \
		fileeditor-extension \
		notebook-extension \
		statusbar-extension \
		toc-extension)
$(HOME)/.jupyter/jupyter_notebook_config.py: $(HOME)/.jupyter/%: jupyter/%
$(addprefix $(HOME)/.jupyter/lab/user-settings/@jupyterlab/, \
		application-extension \
		apputils-extension \
		codemirror-extension \
		docmanager-extension \
		extensionmanager-extension \
		fileeditor-extension \
		notebook-extension \
		statusbar-extension \
		toc-extension \
		): $(HOME)/.jupyter/lab/user-settings/@jupyterlab/%: jupyter/%

x-config: $(addprefix $(HOME)/,.xinitrc .xmodmaprc)
$(HOME)/.xinitrc: x/xinitrc
$(HOME)/.xmodmaprc: x/xmodmaprc

firejail-config: $(HOME)/.config/firejail
$(HOME)/.config/firejail: firejail

firefox-config: $(HOME)/bin/ff
$(HOME)/bin/ff: bin/ff

font-config: $(HOME)/.config/fontconfig/fonts.conf
$(HOME)/.config/fontconfig/fonts.conf: util/fonts.conf

$(HOME)/bin/totp: bin/totp

# Symlink dotfile.
$(addprefix $(HOME)/, \
        .config/xfce4/terminal/terminalrc \
        .config/lxterminal/lxterminal.conf \
        .autoenv.zsh \
        $(addprefix .config/git/,config ignore attributes) \
        bin/ssh-agent-connect \
        $(addprefix .gnupg/,gpg.conf gpg-agent.conf dirmngr.conf) \
        .inputrc \
        .colordiffrc \
        .config/htop/htoprc \
        bin/lesspipe \
        .config/source-highlight/esc.style .local/share/source-highlight \
        .unison/default.prf \
        .editorconfig \
        .mypy.ini .flake8 .pylintrc .isort.cfg \
		.jupyter/jupyter_notebook_config.py \
		$(addprefix .jupyter/lab/user-settings/@jupyterlab/, \
			application-extension \
			apputils-extension \
			codemirror-extension \
			docmanager-extension \
			extensionmanager-extension \
			fileeditor-extension \
			notebook-extension \
			statusbar-extension \
			toc-extension) \
        .xinitrc .xmodmaprc \
        .config/firejail \
        .config/fontconfig/fonts.conf \
        bin/ff \
        bin/totp):
	mkdir -p "$$(dirname $@)"
	ln -sfT "$$(bin/relative-path "$$(pwd)/$<" "$$(dirname "$@")")" "$@"

# Generate dotfile by concatenation.
$(addprefix $(HOME)/, \
        .emacs.d/init.el \
        .zprofile .zshrc \
        .bash_profile .bashrc \
        .tmux.conf):
	rm -f "$@"
	mkdir -p "$$(dirname $@)"
	cat $^ > "$@"
	chmod -w "$@"

$(HOME)/.zplug: | git
	git clone https://github.com/zplug/zplug "$@"

# Install diff-highlight (for git diff) by patching files in the upstream repo.
$(HOME)/bin/diff-highlight: | curl
	rm -f $(HOME)/.local/share/perl5/lib/perl5/DiffHighlight.pm
	mkdir -p $(HOME)/.local/share/perl5/lib/perl5
	curl -fsS \
	    https://raw.githubusercontent.com/git/git/master/contrib/diff-highlight/DiffHighlight.pm \
	    > $(HOME)/.local/share/perl5/lib/perl5/DiffHighlight.pm
	chmod -w $(HOME)/.local/share/perl5/lib/perl5/DiffHighlight.pm
	rm -f "$@"
	mkdir -p "$$(dirname $@)"
	echo -e \
	    "#!/usr/bin/perl\n\nuse lib '$(HOME)/.local/share/perl5/lib/perl5';\nuse DiffHighlight;\n" \
	    > "$@"
	curl -fsS \
	    https://raw.githubusercontent.com/git/git/master/contrib/diff-highlight/diff-highlight.perl \
	    >> "$@"
	chmod -w+x "$@"

.PHONY: dotfiles-clean

dotfiles-clean:
	rm -f $(addprefix $(HOME)/, \
	    .emacs.d/init.el \
	    .zprofile .zshrc .autoenv.zsh \
	    $(addprefix .config/git/,config ignore attributes) bin/diff-highlight \
	    .tmux.conf \
	    .inputrc \
	    .colordiffrc \
	    .config/htop/htoprc \
	    bin/lesspipe \
	    .config/source-highlight/esc.style .local/share/source-highlight \
	    .unison/default.prf \
	    .editorconfig \
        .mypy.ini .flake8 .pylintrc .isort.cfg)


#  ----------  SETTINGS TARGETS  -----------------------------------------------

ifeq ($(OSTYPE),darwin)

SETTINGS := \
        os-config \
        firefox-config \
        iterm2-config \
        xquartz-config

.PHONY: settings $(SETTINGS)
settings: $(SETTINGS)

os-config: dotmacos
	for file in mac/macos/*.sh; do \
	    if [[ -x "$$file" ]]; then \
	        echo "Dry-running $$file"; \
	    else \
	        echo -e "\033[31mWARNING: Skipping non-executable $$file.\033[0m"; \
	    fi; \
	done; \
	python3 -m dotmacos.cli set --format json5 mac/macos/*.json5

.PHONY: dotmacos
dotmacos: python3
	if ! python3 -m dotmacos.cli &>/dev/null 2>&1; then \
	    if [[ -n "$$VIRTUAL_ENV" ]]; then \
	        echo -e '\033[31mERROR: Cannot install dotmacos in a virtual' \
	            'env.\033[0m' "Please run 'deactivate' first."; \
	        exit 1; \
	    fi; \
	    python3 -m pip install \
	        'git+https://github.com/shvenkat/dotmacos#egg=dotmacos'; \
	fi

firefox-config:
	# if pgrep -U "$$(id -u)" -x firefox >/dev/null 2>&1; then \
	# 	echo -e '\033[31mERROR: Cannot install Firefox preferences' \
	#         'because Firefox is running. Quit the app and try again.'; \
	#     exit 1; \
	# fi
	profiles_dir="$$(HOME)/Library/Application Support/Firefox/Profiles"; \
	download_dir="$$(HOME)/Downloads"; \
	default_profile="$$(find "$$profiles_dir" -mindepth 1 -maxdepth 1 -type d -name '*.default')"; \
	if [[ ! -d "$$default_profile" ]]; then
	    echo -e '\033[31mWARNING: No default Firefox profile directory found.\033[0m' \
	        'Run Firefox and try again.'; \
	else \
	    firefox_user_js="$${default_profile}/user.js"; \
	    sed -e "s#DOWNLOAD_DIR#$${download_dir}#" < firefox/user.js > "$$firefox_user_js"; \
	fi

iterm2-config: dotmacos
	python3 -m dotmacos.cli set --format json5 mac/iterm2/iterm2.json5
	profile_dir="$(HOME)/Library/Application Support/iTerm2/DynamicProfiles"; \
	mkdir -p "$$profile_dir"; \
	for file in \
	        mac/iterm2/profiles/0_base.json5 \
	        mac/iterm2/profiles/1_lightbox_light.json5 \
	        mac/iterm2/profiles/1_solarized_light.json5 \
	; do \
	    cat "$$file" \
	        | python3 -c 'import sys, pyjson5, json; sys.stdout.write(json.dumps(pyjson5.load(sys.stdin)));' \
	        | jq . \
	        > "$${profile_dir}/$$(basename "$${file}" .json5).json"; \
	done

xquartz-config: dotmacos
	python3 -m dotmacos.cli set --format json5 mac/xquartz.json5

else
SETTINGS :=
endif


#  ----------  PROGRAM TARGETS  ------------------------------------------------

PROGS := \
        coreutils \
        emacs-prog \
        zsh-prog \
        tmux-prog \
        colordiff-prog \
        htop-prog \
        less-prog \
        source-highlight-prog \
        unison-prog \
        pandoc-prog \
        gnupg-prog \
        python-packages \
        R-prog \
        coq-prog \
        isabelle-prog \
        tex-prog \
        shellcheck-prog \
        docker-prog \
        firefox-prog \
        racket-prog

ifeq ($(OSTYPE),darwin)
PROGS += \
        iterm-prog \
        mactimer
endif

.PHONY: progs $(PROGS)
progs: $(PROGS)

ifeq ($(OSTYPE),darwin)
coreutils: brew
	if ! which shuf &>/dev/null; then \
	    brew install coreutils ; \
	    brew install dos2unix; \
	    brew install findutils --with-default-names; \
	    brew install grep --with-default-names; \
	    brew install gzip; \
	    brew install jq; \
	    brew install tree; \
	    brew install watch; \
	    brew install wdiff; \
	fi
endif

emacs-prog: emacs-config
ifeq ($(OSTYPE),darwin)
emacs-prog: brew
	if ! which emacs &>/dev/null; then \
	    brew install emacs --with-cocoa --with-imagemagick@6 \
	        --with-librsvg --with-modules --with-gnutls; \
	fi
else
emacs-prog:
	if ! which emacs &>/dev/null; then \
	    echo "Error: Please figure out how to install emacs."; \
	    exit 1; \
	fi
endif

emacs-ext: emacs-prog
	emacs -nw --kill

zsh-prog: zsh-config
ifeq ($(OSTYPE),darwin)
zsh-prog: brew
	if ! which zsh &>/dev/null; then \
	    brew install zsh zsh-completions; \
	fi
else
zsh-prog:
	if ! which zsh &>/dev/null; then \
	    echo "Error: Please figure out how to install zsh."; \
	    exit 1; \
	fi
endif

zsh-ext: zsh-prog
	if zsh -c "source $(HOME)/.zshrc && which zplug" &>/dev/null; then \
	    zplug="source $(HOME)/.zshrc && zplug"; \
	    zsh -c "$$zplug check" || zsh -c "$$zplug install"; \
	    zsh -c "$$zplug update"; \
	    zsh -c "$$zplug clean"; \
	    zsh -c "$$zplug clear"; \
	fi

less-prog: source-highlight-prog

ifeq ($(OSTYPE),darwin)
tmux-prog \
        colordiff-prog \
        htop-prog \
        less-prog \
        source-highlight-prog \
        unison-prog \
        pandoc-prog \
        R-prog \
        shellcheck-prog \
        : %-prog: %-config brew
	which $* &>/dev/null || brew install $*
endif

ifeq ($(OSTYPE),darwin)
gnupg-prog: brew
	which gpg &>/dev/null || brew install gnupg
endif

python-packages: python3
	if [[ -n "$$VIRTUAL_ENV" ]]; then \
	    echo -e '\033[31mERROR: Python virtual env is active.\033[0m' \
	        "Please run 'deactivate' first."; \
	    exit 1; \
	fi; \
	python3 -m pip install --user --progress-bar no \
	    mypy flake8 pylint \
	    black isort \
	    sphinx sphinx-argparse sphinx-autodoc-typehints sphinx-click \
		click pyyaml \
	    jupyterlab matplotlib numpy pandas scipy seaborn

ifeq ($(OSTYPE),darwin)
coq-prog: brew
	which coqc &>/dev/null || brew install coq
endif

ifeq ($(OSTYPE),darwin)
isabelle-prog: brew
	if ! test -x /Applications/Isabelle2014.app/Contents/Resources/Isabelle2014/bin/isabelle; then \
	    brew tap shvenkat/extra; \
	    brew cask install shvenkat/extra/isabelle2014; \
	fi
endif

ifeq ($(OSTYPE),linux)
.PHONY: tex-prog
else ifeq ($(OSTYPE),darwin)
tex-prog: brew
	test -x /Library/TeX/texbin/tex || brew cask install mactex
endif

ifeq ($(OSTYPE),darwin)
docker-prog: brew
	which docker &>/dev/null || brew cask install docker
endif

ifeq ($(OSTYPE),darwin)
firefox-prog: brew
	test -x /Applications/Firefox.app/Contents/MacOS/firefox \
	    || brew cask install firefox
endif

ifeq ($(OSTYPE),darwin)
racket-prog: brew
	command -v racket &>/dev/null || brew install minimal-racket
endif

ifeq ($(OSTYPE),darwin)
iterm-prog: brew
	test -x /Applications/iTerm.app/Contents/MacOS/iTerm2 \
	    || brew cask install iterm2

mactimer: $(HOME)/bin/mactimer
$(HOME)/bin/mactimer: bin/mactimer
	mkdir -p "$$(dirname $@)"
	ln -sfT "$$(bin/relative-path "$$(pwd)/$<" "$$(dirname "$@")")" "$@"
endif

todo: $(HOME)/bin/todo
$(HOME)/bin/todo: bin/todo python3
	mkdir -p "$$(dirname $@)"
	ln -sfT "$$(bin/relative-path "$$(pwd)/$<" "$$(dirname "$@")")" "$@"


#  ----------  FONT TARGETS  ---------------------------------------------------

FONTS := \
        font-fira-code \
        font-dejavu-mono \
        font-droid \
        font-ebgaramond \
        font-noto \
        font-source-serif

.PHONY: fonts $(FONTS)
fonts: $(FONTS)

ifeq ($(OSTYPE),linux)

FONTS_DIR := $(HOME)/.local/share/fonts

font-fira-code: $(FONTS_DIR)/firacode/FiraCode-Retina.otf
$(FONTS_DIR)/firacode/FiraCode-Retina.otf:
	url="https://github.com/tonsky/FiraCode/releases/download/1.207/FiraCode_1.207.zip"; \
	sha="29615bba3b70641e8f78ca865f8325a3d8dbeb7a8c72288d1e520d4c82e749118f30980b847d35d3fd120914cf4ca1a9c4f57725c8791ba6291a13e8c52dd1d0"; \
	dir="$$(dirname "$@")"; \
	zip="$${dir}/FiraCode.zip"; \
	mkdir -p "$$dir" \
	&& curl -fsSL -o "$$zip" "$$url" \
	&& [[ "$$sha" == "$$(sha512sum -b "$$zip" | cut -d' ' -f1)" ]] \
	&& unzip -q -o -j "$$zip" 'otf/*.otf' -d "$$dir" \
	&& rm -f "$$zip"

font-ebgaramond: $(FONTS_DIR)/ebgaramond/EBGaramond-Regular.otf
$(FONTS_DIR)/ebgaramond/EBGaramond-Regular.otf:
	repo="EBGaramond12"; \
	commit="8e8d2aff04abb39fceca0b1e8da1f444e466fb3b"; \
	url="https://github.com/octaviopardo/$${repo}/archive/$${commit}.tar.gz"; \
	sha="a846b2d0b9931a4bc13e1bcfab60d65b8e90caf5cc11829ad2180cb88ac7e91f15642bdeaea280dd919c7da15fd4aea3d1635667507c676d31fb4c0e09c633a4"; \
	dir="$$(dirname "$@")"; \
	tar="$${dir}/ebgaramond.tar.gz"; \
	mkdir -p "$$dir" \
	&& curl -fsSL -o "$$tar" "$$url" \
	&& [[ "$$sha" == "$$(sha512sum -b "$$tar" | cut -d' ' -f1)" ]] \
	&& tar --strip-components 3 --wildcards \
	    -C "$$dir" -xzf "$$tar" "$${repo}-$${commit}/fonts/otf/*.otf" \
	&& rm -f "$$tar"

font-noto: $(FONTS_DIR)/noto/NotoSans-Regular.ttf
font-noto: $(FONTS_DIR)/noto/NotoEmoji-Regular.ttf

$(FONTS_DIR)/noto/NotoSans-Regular.ttf:
	repo="noto-fonts"; \
	commit="20bc5918912503bc1537a407a694738c33c048aa"; \
	url="https://github.com/googlefonts/$${repo}/archive/$${commit}/$${repo}.tar.gz"; \
	sha="cc087f7cf4357ddda922e2ed0eb2071e5f0173b3779bb620813d71c7cc89c9c2a30a0c2d150b928071efc839c0cb1a142ed7ea28018c5b4879cbf87b6807d31f"; \
	dir="$$(dirname "$@")"; \
	tar="$${dir}/$${repo}.tar.gz"; \
	mkdir -p "$$dir" \
	&& wget -q -O "$$tar" "$$url" \
	&& [[ "$$sha" == "$$(sha512sum -b "$$tar" | cut -d' ' -f1)" ]] \
	&& tar --strip-components 4 --wildcards -C "$$dir" -xzf "$$tar" \
	    "$${repo}-$${commit}/hinted/ttf/NotoSansMono/NotoSansMono-Bold.ttf" \
	    "$${repo}-$${commit}/hinted/ttf/NotoSansMono/NotoSansMono-Light.ttf" \
	    "$${repo}-$${commit}/hinted/ttf/NotoSansMono/NotoSansMono-Medium.ttf" \
	    "$${repo}-$${commit}/hinted/ttf/NotoSansMono/NotoSansMono-Regular.ttf" \
	    "$${repo}-$${commit}/hinted/ttf/NotoSans/NotoSans-Bold*.ttf" \
	    "$${repo}-$${commit}/hinted/ttf/NotoSans/NotoSans-Italic.ttf" \
	    "$${repo}-$${commit}/hinted/ttf/NotoSans/NotoSans-Light*.ttf" \
	    "$${repo}-$${commit}/hinted/ttf/NotoSans/NotoSans-Medium*.ttf" \
	    "$${repo}-$${commit}/hinted/ttf/NotoSans/NotoSans-Regular.ttf" \
	    "$${repo}-$${commit}/hinted/ttf/NotoSerif/NotoSerif-Bold*.ttf" \
	    "$${repo}-$${commit}/hinted/ttf/NotoSerif/NotoSerif-Italic.ttf" \
	    "$${repo}-$${commit}/hinted/ttf/NotoSerif/NotoSerif-Light*.ttf" \
	    "$${repo}-$${commit}/hinted/ttf/NotoSerif/NotoSerif-Medium*.ttf" \
	    "$${repo}-$${commit}/hinted/ttf/NotoSerif/NotoSerif-Regular.ttf" \
	    "$${repo}-$${commit}/hinted/ttf/NotoSansSymbols/NotoSansSymbols-Bold.ttf" \
	    "$${repo}-$${commit}/hinted/ttf/NotoSansSymbols/NotoSansSymbols-Light.ttf" \
	    "$${repo}-$${commit}/hinted/ttf/NotoSansSymbols/NotoSansSymbols-Medium.ttf" \
	    "$${repo}-$${commit}/hinted/ttf/NotoSansSymbols/NotoSansSymbols-Regular.ttf" \
	    "$${repo}-$${commit}/hinted/ttf/NotoSansSymbols2/NotoSansSymbols2-Regular.ttf" \
	&& rm -f "$$tar"

$(FONTS_DIR)/noto/NotoEmoji-Regular.ttf:
	repo="noto-emoji"; \
	commit="aac7ccaa4d1dea4543453b96f7d6fc47066a57ff"; \
	url="https://github.com/googlefonts/$${repo}/archive/$${commit}/$${repo}.tar.gz"; \
	sha="d497c4ecaf299ade23b1f6a3bc6e98d7dbbb619e51f067496d4a4dbf71693c96de26263af595c0aae86c63dcea8be190718f5477af645d4338989f25cc3ab848"; \
	dir="$$(dirname "$@")"; \
	tar="$${dir}/$${repo}.tar.gz"; \
	mkdir -p "$$dir" \
	&& wget -q -O "$$tar" "$$url" \
	&& [[ "$$sha" == "$$(sha512sum -b "$$tar" | cut -d' ' -f1)" ]] \
	&& tar --strip-components 2 --wildcards -C "$$dir" -xzf "$$tar" \
	    "$${repo}-$${commit}/fonts/NotoEmoji-Regular.ttf" \
	    "$${repo}-$${commit}/fonts/NotoColorEmoji.ttf" \
	&& rm -f "$$tar"

else ifeq ($(OSTYPE),darwin)

font-fira-code: brew font-tap
	test -e "$(HOME)/Library/Fonts/FiraCode-Regular.otf" \
	    || brew cask install font-fira-code

font-dejavu-sans: brew font-tap
	test -e "$(HOME)/Library/Fonts/DejaVuSansMono.ttf" \
	    || brew cask install font-dejavu-sans

font-droid-sans-mono: brew font-tap
	test -e "$(HOME)/Library/Fonts/DroidSansMono.ttf" \
	    || brew cask install font-droid-sans-mono

.PHONY: font-tap
font-tap:
	brew tap homebrew/cask-fonts

endif

#  ----------  WRAPPER TARGETS  ------------------------------------------------

emacs: emacs-config emacs-prog emacs-ext
zsh: zsh-config zsh-prog zsh-ext


#  ----------  TOP-LEVEL TARGETS  ----------------------------------------------

.PHONY: help
.SILENT: help
help:
	echo
	echo "SYNPOSIS"
	echo
	echo "    make prereq      Checks and/or install basic requirements."
	echo
	echo "    make all         Install and/or update apps, fonts and config."
	# echo "    make update      Update apps, fonts and config."
	# echo "    make clean       Uninstall apps, fonts and config."
	echo
	echo "    make config      Install dotfiles and app/OS settings."
	echo "    make dotfiles    Install dotfiles."
	echo "    make settings    Install OS and GUI application settings."
	echo
	echo "    make dotfiles-clean     Removes installed dotfiles."
	echo
	echo "    make TARGET [TARGET...] Install/update specific targets (below)."
	echo
	echo "DESCRIPTION"
	echo
	echo "    Installs and/or updates various programs and config files."
	echo
	echo "    Dotfiles: dotfiles are \"installed\" in the home directory either"
	echo "    as symlinks to files in this repo, or generated by concatenation"
	echo "    and/or patching of files from this repo or an online source."
	echo
	echo "TARGETS:"
	echo
	echo "    dotfiles"
	$(foreach target,$(DOTFILES),echo "        $(target)";)
	echo "    settings"
	$(foreach target,$(SETTINGS),echo "        $(target)";)
	echo "    progs"
	$(foreach target,$(PROGS),echo "        $(target)";)
	echo "    fonts"
	$(foreach target,$(FONTS),echo "        $(target)";)

.PHONY: all

all: config apps fonts

config: dotfiles settings

# update: dotfiles-update apps-update fonts-update

# clean: dotfiles-clean apps-clean fonts-clean
