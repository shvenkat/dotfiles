# Bootstrap installer

## Mac OS

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


# vim Focus Mode

VertSplit
