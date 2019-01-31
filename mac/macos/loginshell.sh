#!/bin/bash

set -e -o pipefail -u

login_shell="$(
    dscl . read "/users/$USER" UserShell | sed -e 's/^UserShell: //'
)"

if which zsh &>/dev/null && [[ "$(basename "$login_shell")" != zsh ]]; then
    echo -e "\033[31mWARNING: Login shell is $login_shell.\033[0m" \
        'To make zsh your login shell, add its path to /etc/shells' \
        "and run 'chsh -s <path>'."
fi
