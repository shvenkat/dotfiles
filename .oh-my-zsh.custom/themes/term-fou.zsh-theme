# LEFT PROMPT
# The prompt is '%% ' if the shell is running without privileges, else '%% '.
# The prompt is green if the last command exited successfully, red otherwise.
PS1='%(?,%{$fg[green]%},%{$fg[red]%})%#%# %{$reset_color%}'

# RIGHT PROMPT
__host="%7>…>%m%>>"
__cwd="%6>…>%1~%>>"

GIT_PS1_SHOWDIRTYSTATE=yes
GIT_PS1_SHOWSTASHSTATE=yes
GIT_PS1_SHOWUNTRACKEDFILES=yes
GIT_PS1_SHOWUPSTREAM="verbose"
GIT_PS1_DESCRIBE_STYLE="branch"
git_status="%{$fg_bold[magenta]%}"'$(__git_ps1 "(%s)")'

RPS1='${__host} ${__cwd} ${(e)${git_status}}%{$reset_color%}'
