# Script source-d by zsh-autoenv plugin when entering/leaving a directory.
# Copied from:
# https://github.com/Tarrasch/zsh-autoenv/blob/master/README.md#automatically-activate-python-virtualenvs.
#
# This script currently does the following:
#   - Activates and deactivates a Python virtual environment using a .venv or
#     env directory located in the current directory or one of its ancestors.
#     The "nearest" matching directory is used. Irrespective of "distance",
#     .venv directories take precendence over env.

if [[ $autoenv_event == 'enter' ]]; then
  autoenv_source_parent

  _my_autoenv_venv_chpwd() {
    # Do nothing if the virtual environment was entered manually.
    if [[ -z "$_ZSH_ACTIVATED_VIRTUALENV" && -n "$VIRTUAL_ENV" ]]; then
      return
    fi

    # Find the first .venv/ directory up the filesystem tree. If none are found,
    # find the first env/ directory up the filesystem tree. Use absolute paths.
    setopt localoptions extendedglob
    local -a venv
    venv=(./(../)#.venv(NY1:a))
    if ! (( $#venv )); then
        venv=(./(../)#env(NY1:a))
    fi

    # If a virtual environment is active, deactivate it unless it is the one to
    # be activated below (to avoid re-activating the same environment).
    if [[ -n "$_ZSH_ACTIVATED_VIRTUALENV" && -n "$VIRTUAL_ENV" ]]; then
      if ! (( $#venv )) || [[ "$_ZSH_ACTIVATED_VIRTUALENV" != "$venv[1]" ]]; then
        unset _ZSH_ACTIVATED_VIRTUALENV
        # echo "De-activating virtualenv: ${(D)VIRTUAL_ENV}" >&2
        deactivate
      fi
    fi

    # If a virtual environment is not active, activate a suitable one if found.
    if [[ -z "$VIRTUAL_ENV" ]]; then
      if (( $#venv )); then
        # echo "Activating virtualenv: ${(D)venv}" >&2
        source $venv[1]/bin/activate
        _ZSH_ACTIVATED_VIRTUALENV="$venv[1]"
      fi
    fi
  }

  autoload -U add-zsh-hook
  add-zsh-hook chpwd _my_autoenv_venv_chpwd
  _my_autoenv_venv_chpwd
else
  add-zsh-hook -d chpwd _my_autoenv_venv_chpwd
fi
