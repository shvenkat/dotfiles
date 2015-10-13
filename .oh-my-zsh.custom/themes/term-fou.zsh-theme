PS1='%(?,%{$fg[green]%},%{$fg[red]%})%h %% '
RPS1='%{$reset_color%}%7>…>%m%>>:%6>…>%1~%>>$(git_prompt_info)%{$reset_color%}'

ZSH_THEME_GIT_PROMPT_PREFIX=" %{$fg_bold[magenta]%}("
ZSH_THEME_GIT_PROMPT_SUFFIX=")%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CLEAN=""
ZSH_THEME_GIT_PROMPT_UNSTAGED="*"
ZSH_THEME_GIT_PROMPT_STAGED="+"
ZSH_THEME_GIT_PROMPT_UNTRACKED="%%"
ZSH_THEME_GIT_PROMPT_STASHED="$"
ZSH_THEME_GIT_PROMPT_UPSTREAM="git verbose"
