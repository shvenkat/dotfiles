#!/bin/bash

set -e
set -o pipefail

INSTALL_DIR="$HOME/Library/Application Support/iTerm2/DynamicProfiles"
SOURCE_DIR=$(dirname $0)

REPO=$(git rev-parse --show-toplevel)
YAML2JSON=${REPO}/bin/proteus/yaml2json.sh


# Converts YAML profile to JSON and installs.
# $1: profile YAML in the same directory as this script.
install_profile () {
    profile_yaml=$1
    profile_json="$(basename $profile_yaml .yaml).json"
    cat ${SOURCE_DIR}/${profile_yaml} \
    | $YAML2JSON \
    > "${INSTALL_DIR}/${profile_json}"
}


# Main
ls ${SOURCE_DIR}/*.yaml \
| while read profile_yaml; do
    install_profile $profile_yaml
done
