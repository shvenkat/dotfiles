# BASE CONFIG ---------------------------------------------------------------

# export LANG=en_US.UTF-8


# ADDITIONAL CONFIG MODULES -------------------------------------------------

case $(uname) in
Linux)  OSENV=linux ;;
Darwin) OSENV=macosx ;;
Cygwin) OSENV=cygwin ;;
*)      OSENV=unknown ;;
esac
. ~/.rc/.bashrc.d/bash
. ~/.rc/.zshrc.d/path
. ~/.rc/.zshrc.d/alias
. ~/.rc/.zshrc.d/less
. ~/.rc/.zshrc.d/ls
. ~/.rc/.zshrc.d/ssh
. ~/.rc/.zshrc.d/term
. ~/.rc/.zshrc.d/misc
. ~/.rc/.bashrc.d/completion
. ~/.rc/.bashrc.d/hist
. ~/.rc/.bashrc.d/prompt
unset OSENV

# for more config options, see http://www.linuxselfhelp.com/HOWTO/Config-HOWTO/config.html

# local settings
export EDITOR=vim
