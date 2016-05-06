"""Convert YAML on stdin to JSON on stdout"""

import sys
import ruamel.yaml as yaml
import json

json.dump(yaml.load(sys.stdin.read()), sys.stdout)
