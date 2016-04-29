% The Overcast Palettes For Text Terminals
% Shiv Venkatasubrahmanyam
% 2016-04-01


# SUMMARY

These pallettes are derived from Ethan Schoonover's [solarized]
(http://ethanschoonover.com/solarized) palette, and are intended for use with
ergonomic display contrast and color temperature settings. The name is a play
on the original and a reference to an overcast day.

The light palette has been selected to improve clarity when display brightness
is close to ambient light level and display color temperature approximates
daylight (6500 K). The grayscale is identical to that of solarized light. Most
colors have been adjusted to increase the perceived difference between the hues
while moderating the brightness contrast with foreground text.

Similarly, the dark palette has been optimized for the lowest display
brightness, suited to working in a dark room, and a lower color temperature
(3400 K), which is appropriate for night time. The grayscale has been adjusted
to slightly increase contrast between foreground elements and background.
Colors have been adjusted for the same reasons as in the case of the light
palette.


# THE LIGHT PALETTE

NAME       16/8  TERMCOL    L*A*B       HSB
---------  ----  ---------  ----------  -----------
highlight   8/4  brblack    15 -12 -12  193 100  21
focus       0/4  black      20 -12 -12  192  90  26
emphasis   10/7  brgreen    45 -07 -07  194  25  46
text       11/7  bryellow   50 -07 -07  195  23  51
note       12/6  brblue     60 -06 -03  186  13  59
comment    14/4  brcyan     65 -05 -02  180   9  63
tint        7/7  white      92 -00  10   44  11  93
backgrnd   15/7  brwhite    97  00  10   44  10  99

violet     13/5  brmagenta  50  15 -45  237  45  77
blue        4/4  blue       54 -03 -44  205  82  80
cyan        6/6  cyan       57 -33 -04  175  74  60
green       2/2  green      60 -38  62   80 100  63
yellow      3/3  yellow     59 -01  64   50 100  66
orange      9/3  brred      54  38  57   25  89  80
red         1/1  red        55  76  54  359  85 100
magenta     5/5  magenta    52  58 -29  310  60  76


# THE DARK PALETTE

NAME       16/8  TERMCOL    L*A*B       HSB
---------  ----  ---------  ----------  -----------
highlight  15/7  brwhite    97  00  10   44  10  99
focus       7/7  white      92 -00  10   44  11  93
emphasis   14/4  brcyan     76 -06 -02  180   9  75
text       12/6  brblue     65 -05 -02  180   9  63
note       11/7  bryellow   60 -06 -03  186  13  59
comment    10/7  brgreen    50 -07 -07  195  23  51
tint        0/4  black      20 -12 -12  192  90  26
backgrnd    8/4  brblack    15 -12 -12  193 100  21

violet     13/5  brmagenta  58  21 -46  238  42  86
blue        4/4  blue       67  00 -52  208  74 100
cyan        6/6  cyan       65 -29 -04  175  52  67
green       2/2  green      65 -34  65   76  93  67
yellow      3/3  yellow     58 -01  63   50 100  65
orange      9/3  brred      54  38  57   25  89  80
red         1/1  red        55  76  54  359  85 100
magenta     5/5  magenta    50  54 -26  312  58  73

TODO: balance the brightness of the colors for the dark palette.


# COLOR MAPPING

## Terminal

background  backgrnd
foreground  text
cursor      highlight
emphasis foreground   highlight
selection background  highlight
selection foreground  note

## Diff

same        text
old         magenta
new         cyan
hunk/function  green
header      yellow

## Code viewing/editing

Constant    cyan
Keyword     green
Function    blue
Type        yellow
Error       orange
Warning     magenta
