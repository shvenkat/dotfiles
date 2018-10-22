SHELL:=BASH_ENV= /bin/bash -e -u -o pipefail -c


xterm-16color-italics:
	infocmp xterm-16color \
	| sed -e 's/xterm-16color|[^,]*,/xterm-16color-italics|xterm with 16 colors and italics,/' \
	| cat - <(echo '	sitm=\E[3m, ritm=\E[23m,') \
	| tic -o ~/.terminfo -

yterm-16color-nomouse:
	infocmp xterm-16color \
	| sed -e 's/xterm-16color|[^,]*,/yterm-16color-nomouse|xterm with 16 colors but no mouse support,/' \
	      -e 's/kmous=[^,]*, //' \
	| tic -o ~/.terminfo -
