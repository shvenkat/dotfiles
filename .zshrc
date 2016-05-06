# BASE CONFIG ---------------------------------------------------------------

# export LANG=en_US.UTF-8


# OH-MY-ZSH CONFIG ----------------------------------------------------------

export ZSH=~/.rc/.oh-my-zsh
ZSH_THEME="term-fou"
# CASE_SENSITIVE="true"
DISABLE_AUTO_UPDATE="true"
# export UPDATE_ZSH_DAYS=13
# DISABLE_LS_COLORS="true"
DISABLE_AUTO_TITLE="true"
# ENABLE_CORRECTION="true"
# COMPLETION_WAITING_DOTS="true"
# DISABLE_UNTRACKED_FILES_DIRTY="true"
HIST_STAMPS="yyyy-mm-dd"
ZSH_CUSTOM=~/.rc/.oh-my-zsh.custom
plugins=(gitfast)
# plugins=(git-dashboard zsh-syntax-highlighting autoenv common-aliases
# dircycle fasd fastfile git-extras gitfast jump ssh-agent vi-mode
# virtualenv zsh-autosuggestions predict-on)
source $ZSH/oh-my-zsh.sh
# ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=cyan,bold'


# ADDITIONAL CONFIG MODULES -------------------------------------------------

case $(uname) in
Linux)  OSENV=linux ;;
Darwin) OSENV=macosx ;;
Cygwin) OSENV=cygwin ;;
*)      OSENV=unknown ;;
esac
. ~/.rc/.zshrc.d/zsh
. ~/.rc/.zshrc.d/path
. ~/.rc/.zshrc.d/alias
. ~/.rc/.zshrc.d/less
. ~/.rc/.zshrc.d/ls
. ~/.rc/.zshrc.d/ssh
. ~/.rc/.zshrc.d/term
. ~/.rc/.zshrc.d/keys
. ~/.rc/.zshrc.d/misc
unset OSENV
