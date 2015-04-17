export ZSH=~/.oh-my-zsh
ZSH_THEME="terminalparty"
# CASE_SENSITIVE="true"
DISABLE_AUTO_UPDATE="true"
# export UPDATE_ZSH_DAYS=13
# DISABLE_LS_COLORS="true"
DISABLE_AUTO_TITLE="true"
# ENABLE_CORRECTION="true"
# COMPLETION_WAITING_DOTS="true"
# DISABLE_UNTRACKED_FILES_DIRTY="true"
HIST_STAMPS="yyyy-mm-dd"
# ZSH_CUSTOM=/path/to/new-custom-folder
plugins=(git zsh-syntax-highlighting)
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

GIT_PS1_SHOWDIRTYSTATE=true
GIT_PS1_SHOWSTASHSTATE=true
GIT_PS1_SHOWUNTRACKEDFILES=true
GIT_PS1_SHOWUPSTREAM='auto verbose git'
. ~/.rc/.git-completion.sh
# PS1=$PS1$BR_MAGENTA'$(__git_ps1 "(%s)")'$NONE"\n> "
# PROMPT='%(?,%{$fg[green]%},%{$fg[red]%}) >> '
# RPS1='%{$fg[white]%}%2~ %{$fg_bold[magenta]%}$(__git_ps1 "(%s)")%{$reset_color%}'

