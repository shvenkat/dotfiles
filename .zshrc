export ZSH=~/.oh-my-zsh
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
plugins=(git git-dashboard zsh-syntax-highlighting)
source $ZSH/oh-my-zsh.sh

# export LANG=en_US.UTF-8
. ~/.rc/.aliases
case $(uname) in
Linux)
  OSENV=linux
  ;;
Darwin)
  OSENV=macosx
  ;;
Cygwin)
  OSENV=cygwin
  ;;
esac
BASHRCD=$HOME/.rc/.bashrc.d
# . $BASHRCD/.bashrc.system
# . $BASHRCD/.bashrc.history
# . $BASHRCD/.bashrc.prompt
. $BASHRCD/.bashrc.lscolor
. $BASHRCD/.bashrc.path
. $BASHRCD/.bashrc.ssh
# . $BASHRCD/.bashrc.completion
# . $BASHRCD/.bashrc.misc
. $BASHRCD/.bashrc.terminal
