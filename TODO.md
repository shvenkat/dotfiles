# Bootstrap installer

## Mac OS

### Preferences

Requirements:

  * Read and set preferences from readable, commented config file.
  * Get, filter, merge and write preferences to config file, preserving
    comments.
  * Preferences set should take effect immediately and persist (not be
    overwritten by OS or app).

Design:

  * YAML as the config file format. Not StrictYAML (every value is a string and
    needs conversion to appropriate types). Not HJSON (python library does not
    preserve comments). Others (JSON5, HOCON) do not preserve comments.
  * Python package with CLI to ease reading and maintaining code.
  * ruamel.yaml as YAML library to preserve comments.
  * Use the defaults command to get/set preferences. This is the recommended
    interface for scripts to use the preference system and updates the cfprefsd
    cache.
  * Close apps before setting to prevent apps from ignoring or over-writing the
    updated preferences.

mac-prefs (get|set) --yaml foo.yaml [--yaml bar.yaml [...]]

Usage patterns:

  * Single YAML file, install, dump with filter/merge/comments.
  * Multiple YAML files composed into one, install, dump.
  * Multiple individual YAML files, install, dump separately.

Preferences evolve. Even if the user sources a YAML from a remote source, she/he
may want to update it with local modifications.

Do users need this in the age of cloud preference sync-ing?

## Mac apps

### Firefox config


# lesspipe

* Handles all inputs including stdin and with failsafe behavior.
* Configurable map of file types and handlers.
  *.py text/plain;python    highlight
  *.c *.h text/plain;C      highlight
  *.sqlite3                 peek_db
  *.json                    jq
  *.log                     highlight Error and Warn lines
  ...
  highlight     source-highlight or highlight or pygmentize or code2color
* Allow chaining handlers. untar followed by syntax-highlight.
* Powerful file type inference for stdin.
* Suggested map and handlers.
