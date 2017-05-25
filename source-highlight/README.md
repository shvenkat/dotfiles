PATCHED source-highlight CONFIGURATION FILES

The syntax highlighter source-highlight is useful, flexible and fast but not actively maintained. A
bug report has been open with no action or even triage for over five months. The last two patch
releases where two and five years ago. This directory contains patched configuration files that can
be used in place of those bundled with the software release.


# Installation and Use

1.  Copy the contents of this directory to your machine, say to the location
    `~/.config/source-highlight`.
2.  Call `source-highlight` with the arguments `--data-dir ~/.config/source-highlight`.


# Patches

## source-highlight does not terminate on .zsh input files

Bug Report: https://savannah.gnu.org/patch/?9144

Patch derived from the bug report:
```
diff -u -r source-highlight/3.1.8/share/source-highlight .
--- source-highlight/3.1.8/share/source-highlight/zsh.lang
+++ ./zsh.lang
@@ -23,7 +23,7 @@
           "compcall|compctl|compdescribe|compfiles|compgroups",
           "compquote|comptags|comptry|compvalues|continue",
           "declare|dirs|disable|disown|do|echo|echotc|echoti",
-          "elif|else|emulate|enable|esac|eval|exec|exec|exit",
+          "elif|else|emulate|enable|esac|eval|exec|exit",
           "export|false|fc|fg|fi|float|for|foreach|function",
           "functions|getcap|getln|getopts|hash|history|if|in",
           "integer|jobs|kill|let|limit|local|log|logout",
@@ -35,7 +35,7 @@
           "unlimit|unset|unsetopt|until|vared|wait|whence",
           "where|which|while|zcompile|zformat|zftp|zle",
           "zmodload|zparseopts|zprof|zpty|zregexparse",
-          "zsocket|zstyle|ztcp|"
+          "zsocket|zstyle|ztcp"

 variable = '\$\{([^[:blank:]]+)\}'
 variable = '\$\(([^[:blank:]]+)\)'
```
