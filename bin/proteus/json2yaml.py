"""Convert JSON on stdin to YAML on stdout"""

import sys
import ruamel.yaml as yaml
import json

yaml.dump(json.load(sys.stdin), sys.stdout, default_flow_style=False)
