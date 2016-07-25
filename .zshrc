# BASE CONFIG ---------------------------------------------------------------

# export LANG=en_US.UTF-8


# PLUGINS -------------------------------------------------------------------

# See https://github.com/zplug/zplug
export ZPLUG_HOME=~/.zplug
export ZPLUG_PROTOCOL=HTTPS
export ZPLUG_CLONE_DEPTH=1
export ZPLUG_USE_CACHE=true
source ~/.zplug/init.zsh
zplug "zplug/zplug"
zplug "~/.rc/.oh-my-zsh.custom/plugins/rtab", from:local
zplug "plugins/gitfast", from:oh-my-zsh
zplug "plugins/virtualenv", from:oh-my-zsh
zplug "~/.rc/.oh-my-zsh.custom/themes/term-fou.zsh-theme", from:local
zplug "Tarrasch/zsh-autoenv"
# Install plugins if needed
if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi
compinit -i
zplug load
# HIST_STAMPS="yyyy-mm-dd"
# ZSH_CUSTOM=~/.rc/.oh-my-zsh.custom
# plugins=(gitfast rtab)
# # plugins=(git-dashboard zsh-syntax-highlighting autoenv common-aliases
# # dircycle fasd fastfile git-extras gitfast jump ssh-agent vi-mode
# # virtualenv zsh-autosuggestions predict-on)
# source $ZSH/oh-my-zsh.sh
# # ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=cyan,bold'


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
. ~/.rc/.zshrc.d/hist
unset OSENV
