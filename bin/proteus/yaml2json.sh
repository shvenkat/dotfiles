#!/bin/bash

cd $(dirname $0)
test -f env/bin/activate || pyvenv env
source env/bin/activate
python -c "import ruamel.yaml;" || pip install ruamel.yaml
python yaml2json.py
