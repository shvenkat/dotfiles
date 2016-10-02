% The Lightbox Palettes For Text Terminals
% Shiv Venkatasubrahmanyam
% 2016-10-02


# SUMMARY

These palettes are inspired by Ethan Schoonover's [solarized]
(http://ethanschoonover.com/solarized) palette, and are intended for use with
ergonomic display contrast and color temperature settings. The grayscale and
accent colors have been selected using the Munsell color system to meet certain
perceptual requirements.

* The lightness contrast between background, comment, text and emphasis values
  (see below) is readily visible but not conspicuous.
* The accent colors are clearly distinct, muted (pastel) and similar in
  perceived lightness to the default foreground text.
* The grayscale is close to - but not quite - neutral.
* The accent colors approximate "canonical" hues, such as red, yellow and green,
  instead of puce and chartreuse.

The dark palette has been optimized for low display brightness, suitable for
working in dark surroundings. The light palette has been selected to improve
clarity when display brightness is close to ambient light level in a well lit
room or outdoors during the day. In both cases, display color temperature is
assumed to approximate daylight (6500 K).


# THE PALETTES

                                  Dark palette                     Light palette
                                  -----------------------------    -----------------------------
NAME         16/8    TERMCOL      Munsell*          sRGB           Munsell*          sRGB
---------    ----    --------     --------------    -----------    --------------    -----------
highlight    15/7    brwhite      5Y    9.7 /1.3    253 246 228    N     0   /0        0   0   0
focus         7/7    white        5Y    9.3 /1.3    237 230 211    10BG  2   /2       33  53  56
emphasis     14/4    brcyan       10BG  7.75/1.5    181 197 198    10BG  3.5 /2       65  89  92
text         12/6    brblue       10BG  6.25/1.5    142 161 162    10BG  5   /2      102 128 130
note         11/7    bryellow     10BG  5.5 /1.5    121 141 142    10BG  5.75/1.5    127 145 146
comment      10/7    brgreen      10BG  4.75/1.5    101 122 124    10BG  6.5 /1.3    148 164 165
tint          0/4    black        10BG  1.75/1.7     26  46  52    5Y    9.3 /1.3    237 230 211
backgrnd      8/4    brblack      10BG  1.3 /1.7     15  37  43    5Y    9.7 /1.3    253 246 228

blue          4/4    blue         2.5PB 6.25/8      101 161 211    2.5PB 5.25/8       73 135 184
cyan          6/6    cyan         10G   6.25/5       95 169 148    10G   5.5 /7       29 153 128
green         2/2    green        7.5GY 6.5 /8      126 177  83    7.5GY 5.75/8      106 156  64
yellow        3/3    yellow       5Y    6.25/5      174 155  89    5Y    5.5 /7      160 134  39
orange        9/3    brred        2.5YR 6   /6      197 134 104    2.5YR 5.25/8      189 109  68
red           1/1    red          2.5R  6   /12     238 107 119    2.5R  5   /16     232  45  90
magenta       5/5    magenta      10P   6   /7      185 133 172    10P   5.25/8      170 109 156
violet       13/5    brmagenta    10PB  6.25/8      154 149 207    10PB  5.25/8      130 122 181

* Based on Munsell renotation data from
  https://www.rit.edu/cos/colorscience/rc_munsell_renotation.php. Interpolated
  value and chroma are approximate.


# COLOR MAPPING

## Terminal

background            backgrnd
foreground            text
cursor                highlight
emphasis foreground   highlight
selection background  highlight
selection foreground  note

## diff

same    text
old     magenta
new     cyan
hunk/function  blue
header  emphasis

## git

same      text
old       magenta
new       cyan
hunk      blue
function  blue
header    emphasis
commit    reverse

## Syntax highlighting

Comment   comment
Constant  yellow
Keyword   green
Function  blue
Type      violet
Error     orange
Warning   magenta
