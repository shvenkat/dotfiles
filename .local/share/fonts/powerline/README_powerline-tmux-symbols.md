* Download dotted-zero and/or slashed-zero versions of Droid Sans Mono from
  http://www.cosmix.org/software
* Install python 2.7+ and fontforge with python bindings
* Install powerline-fontpatcher from github
* Download powerline-symbols.sfd from powerline-fontpatcher on github
* Add tmux glyph(s) to powerline-symbols.sfd
    In this case, I added a glyph for codepoint U+2502 by copying the relevant
    section from DejaVuSansMono.sfd. The result is not perfect (tmux pane
    borders have a kink near borders) but it's good enough and far more pleasing
    than the dashed line I get without the patched font.
* Run fontpatcher using:
    ~/.local/bin/powerline-fontpatcher ./DroidSansMonoSlashed.ttf
