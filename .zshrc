# BASE CONFIG ---------------------------------------------------------------

# export LANG=en_US.UTF-8


# PLUGIN MANAGEMENT AND CONFIG ----------------------------------------------

# Manage plugins using zplug (https://github.com/zplug/zplug)
export ZPLUG_HOME=~/.zplug
export ZPLUG_PROTOCOL=HTTPS
export ZPLUG_CLONE_DEPTH=1
export ZPLUG_USE_CACHE=true
source ~/.zplug/init.zsh

# Plugin management.
zplug "zplug/zplug"

# Command completion.
# zplug "zsh-users/zsh-autosuggestions"
# ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=cyan,bold'

# Automation.
zplug "Tarrasch/zsh-autoenv"

# Theme (command prompt).
zplug "plugins/shrink-path", from:oh-my-zsh
zplug "plugins/gitfast", from:oh-my-zsh, nice:10
zplug "plugins/virtualenv", from:oh-my-zsh
zplug "shvenkat/zsh-theme-dexter", use:"*.zsh", nice:19
DEXTER_EXIT_SUCCESS_COLOR=$fg[default]
DEXTER_EXIT_FAILURE_COLOR=$fg[red]
DEXTER_VENV_COLOR=$fg_bold[green]
DEXTER_GIT_COLOR=$fg_bold[green]
DEXTER_WORKDIR_COLOR=$fg_bold[green]
DEXTER_HOSTNAME_COLOR=$fg_bold[green]

# Install plugins, if needed.
if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo
        zplug install
    fi
fi

# Load plugins.
zplug load

# Other plugins of interest:
# common-aliases
# dircycle
# fasd
# fastfile
# git-extras
# jump
# predict-on
# ssh-agent
# vi-mode
# zsh-syntax-highlighting


# ADDITIONAL CONFIG MODULES -------------------------------------------------

. ~/.rc/.zshrc.d/zsh
. ~/.rc/.zshrc.d/path
. ~/.rc/.zshrc.d/alias
. ~/.rc/.zshrc.d/less
. ~/.rc/.zshrc.d/ls
. ~/.rc/.zshrc.d/ssh
. ~/.rc/.zshrc.d/term
. ~/.rc/.zshrc.d/keys
. ~/.rc/.zshrc.d/misc
. ~/.rc/.zshrc.d/hist
