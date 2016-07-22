# LEFT PROMPT
# The prompt is '%% ' if the shell is running without privileges, else '## '.
# The prompt is green if the last command exited successfully, red otherwise.
PS1='%(?,%{$fg[green]%},%{$fg[red]%})%#%# %{$reset_color%}'

# RIGHT PROMPT

# Hostname
__host="%7>…>%m%>>"

# Working directory
# See http://unix.stackexchange.com/questions/273529/shorten-path-in-zsh-prompt/273653
# for alternatives in shortening the working directory.
__cwd='$(__rtab -f -T)'

# Python virtualenv
ZSH_THEME_VIRTUALENV_PREFIX="%{$fg[yellow]%}["
ZSH_THEME_VIRTUALENV_SUFFIX="]%{$reset_color%}"
__venv='$(virtualenv_prompt_info)'

# Git status
GIT_PS1_SHOWDIRTYSTATE=yes
GIT_PS1_SHOWSTASHSTATE=yes
GIT_PS1_SHOWUNTRACKEDFILES=yes
GIT_PS1_SHOWUPSTREAM="verbose"
GIT_PS1_DESCRIBE_STYLE="branch"
git_status="%{$fg_bold[magenta]%}"'$(__git_ps1 "(%s)")%{$reset_color%}'

RPS1='${(e)${__venv}} ${(e)${git_status}} ${(e)${__cwd}} ${__host}'
