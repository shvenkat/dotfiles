# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

shopt -s histappend
shopt -s cdspell
export HISTCONTROL="ignoredups"
export HISTSIZE=1000000
export HISTFILESIZE=1000000
export HISTIGNORE=$'[ \t]*:&:[fb]g:exit:ls'
#export PROMPT_COMMAND="history -a"

[ -f $HOME/.aliases ] && . $HOME/.aliases
[ -f $HOME/.aliases.local ] && . $HOME/.aliases.local

## User specific aliases and functions

# prompt
export PROMPT_COMMAND='PS1X=$(p="${PWD#${HOME}}"; [ "${PWD}" != "${p}" ] &&
  echo -n "~"; IFS=/; for q in ${p:1}; do echo -n /${q:0:3}; done; 
  echo -n "${q:3}")'
case $TERM in
    xterm*|screen*)
        export PS1='\[\033]0;\h $PS1X\007\]\[\e[0;36m\][\h \W] \[\e[0m\]'
        ;;
    *)
	export PS1='\[\e[0;36m\][\h \W] \[\e[0m\]'
        ;;
esac

# ls color
if [ "$TERM" = "rxvt-cygwin-native" ]; then 
	eval $( TERM="xterm" dircolors $HOME/.dir_colors )
else
	eval $( dircolors $HOME/.dir_colors )
fi

## PATHs

PATH=/usr/local/sbin:/sbin:/usr/sbin
PATH=${PATH}:${HOME}/bin
PATH=${PATH}:${HOME}/bin/util
PATH=${PATH}:${HOME}/opt/screen-4.1.0/bin
PATH=${PATH}:/scratch/shvenkat/opt/git-1.8.4/bin
PATH=${PATH}:${HOME}/opt/R-2.14.0/bin
PATH=${PATH}:${HOME}/opt/texlive-20120710/2012/bin/x86_64-linux
PATH=${PATH}:/usr/local/bin:/bin:/usr/bin
PATH=${PATH}:/usr/hs/bin:/usr/hs/gcc/bin:/usr/hs/binutils/bin:${HOME}/.cabal/bin
#PATH=${PATH}:/usr/kerberos/bin
PATH=${PATH}:/usr/lib64/qt-3.3/bin
PATH=${PATH}:/usr/NX/bin
export PATH

#export PERL_LOCAL_LIB_ROOT="/home/shvenkat/perl5"
#export PERL_MB_OPT="--install_base /home/shvenkat/perl5"
#export PERL_MM_OPT="INSTALL_BASE=/home/shvenkat/perl5"
#export PERL5LIB="/home/shvenkat/perl5/lib/perl5/x86_64-linux-thread-multi:/home/shvenkat/perl5/lib/perl5"
#export PATH="/home/shvenkat/perl5/bin:$PATH"

LD_LIBRARY_PATH=/usr/local/lib64:/usr/local/lib
LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/usr/lib64/R/lib
#LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/usr/lib/jvm/java-1.6.0-openjdk-1.6.0.0.x86_64/jre/lib/amd64/server
#LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/usr/lib/jvm/java-1.6.0-openjdk-1.6.0.0.x86_64/jre/lib/amd64
#LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/home/shvenkat/bin/lib
#LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/home/shvenkat/ggobi/lib
export LD_LIBRARY_PATH

MANPATH=/home/shvenkat/man
MANPATH=${MANPATH}:/home/shvenkat/share/man
MANPATH=${MANPATH}:/home/shvenkat/opt/screen-4.1.0/man
MANPATH=${MANPATH}:/home/shvenkat/byobu/share/man
MANPATH=${MANPATH}:/home/shvenkat/opt/git-1.7.8.4/man
MANPATH=${MANPATH}:/home/shvenkat/opt/R-2.14.0/share/man
MANPATH=${MANPATH}:/home/shvenkat/perl5/man
MANPATH=${MANPATH}:/home/shvenkat/opt/texlive-20120710/2012/texmf/doc/man
MANPATH=${MANPATH}:/usr/share/man
MANPATH=${MANPATH}:/usr/share/man/en
MANPATH=${MANPATH}:/usr/local/share/man
MANPATH=${MANPATH}:/usr/kerberos/man
export MANPATH

INFOPATH=/home/shvenkat/opt/texlive-20120710/2012/texmf/doc/info
export INFOPATH

export R_HOME=/usr/lib64/R

#TEXMFCNF=/home/shvenkat/texlive-20120710
#TEXMFCNF=${TEXMFCNF}:/home/shvenkat/texlive-20120710/texmf-var/web2c
#TEXMFCNF=${TEXMFCNF}:/home/shvenkat/texlive-20120710/texmf/web2c
#TEXMFDBS=/home/shvenkat/texlive-20120710/texmf
#TEXMFDBS=${TEXMFDBS}:/home/shvenkat/texlive-20120710/texmf-dist
#TEXMFDBS=${TEXMFDBS}:/home/shvenkat/texlive-20120710/texmf-config
#TEXMFDBS=${TEXMFDBS}:/home/shvenkat/texlive-20120710/texmf-var
#export TEXMFCNF TEXMFDBS 

#export PKG_CONFIG_PATH=/home/shvenkat/ggobi/lib/pkgconfig

export EDITOR=vim

# ssh
#if [ -z "$SSH_AUTH_SOCK" ]; then
#    echo "Setting SSH_AUTH_SOCK ..."
#    export SSH_AUTH_SOCK=$(find /tmp -type s -user ${USER} -wholename '/tmp/ssh-*/agent*' 2>/dev/null | head -n1)
#fi
if [[ $HOSTNAME == "tengu.amgen.com" ]]; then
  export SSH_AUTH_SOCK=$HOME/.ssh/.ssh-agent
fi

set completion-ignore-case On

# for more config options, see http://www.linuxselfhelp.com/HOWTO/Config-HOWTO/config.html

# To get bash-completion to work properly in screen/byobu, 
# you'll want to move /etc/profile.d/bash-completion.sh to /usr/local/bin/ 
# and add something like this to your ~/.bashrc file:
# Source bash_completion
#if [ -f /usr/local/bin/bash-completion.sh ] && ! shopt -oq posix; then
#    . /usr/local/bin/bash-completion.sh
#fi
