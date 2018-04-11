% Lightbox: Ergonomic 16-Color Palettes For Text Editors and Terminal Emulators
% Shiv Venkatasubrahmanyam
% 2016-10-02 (last updated 2017-07-06)


# SUMMARY

The Lightbox palettes are designed for *ergonomic reading* in text editors and
terminal emulators, even on commodity laptop and desktop displays. These are a
pair of 16-color palettes, light and dark, for use under bright and dim viewing
conditions, respectively. Each palette combines eight grayscale and eight accent
color elements, which have been carefully selected to appear _clearly distinct
but not distracting_ in typical use.

These palettes are derived from Ethan Schoonover's [solarized], which introduced
the concept of precisely constructed palettes for better reading. I created
Lightbox to address certain shortcomings in solarized that I noticed over
prolonged use, on uncalibrated displays and in less-than-ideal lighting.
Lightbox is suitable for reading and writing text and source code, and for all
users except those with exacting color requirements and hardware to match.


# USE

## Text editor or GUI IDE

Add both palettes to your editor or IDE settings. Use the latter to switch
between palettes.

## Terminal emulator

  * Option 1 (recommended): Add both palettes to your terminal emulator's ANSI
    color settings. Set the value of the TERM environment variable to indicate a
    16-color terminal (e.g. xterm-16color, screen-16color-bce-s). To switch
    between light and dark palettes, use the appropriate feature of your
    emulator (e.g. profile switching). This method has the advantage that you do
    *not* need to switch the color scheme of every terminal application such as
    tmux and vim. The latter should be configured to use the solarized light
    colorscheme _for both palettes_.

  * Option 2: Enable "true color" (24-bit) support in your terminal emulator.
    Add both palettes as separate color schemes to each terminal application and
    select the desired scheme in each application.


# DESIGN GOAL

The goal of this design is to maximize reading clarity and comfort, enabling the
user to better focus on content and quickly locate salient details. In
particular:

  * Body text should be comfortable to read against the background, avoiding
    excess contrast.

  * Emphasized as well as ancillary text should be evident, but not pronounced,
    in appearance.

  * Highlights and accented keywords/phrases should be distinct without
    diverting attention.

Ergonomic display settings are targeted for best results. Specifically:

  * Display brightness approximating ambient light level (lowest at night,
    highest outdoors at midday).

  * Moderate contrast.

  * Neutral color temperature (6500 K) approximating that of daylight.

Use of color temperature adjustment is *not* supported. To make reading more
comfortable at night, use the dark profile instead.


# REQUIREMENTS

The diagram below summarizes the required perceptual relationships between the
elements of each palette along the value (lightness) and hue (color tone)
dimensions. Note that the two grayscale elements that are infrequently used
(Note and Focus) are not shown. The remaining grayscale elements are shown along
the vertical value axis; the accent colors along the horizontal hue axis and
aligned in value with the Text element of the grayscale.


  ┬ Highlight
  │
  │  Emphasis                            HUE
V │              ├─────────────────────────────────────────────────────┤
A │      Text <> Red  Green  Yellow  Blue  Magenta  Cyan  Violet  Orange
L │
U │   Comment
E │
  │
  │      Tint
  ┴  Backgrnd



The palettes strike a balance among the following perceptual requirements. The
comparison to solarized is noted where relevant.

1.  Moderate contrast between foreground and background values, as in solarized.

2.  The value of commonly used grayscale elements should match their intented
    use.

      * Tint - used as a fill color for status bars and margins - should be
        close to Backgrnd and far from Comment.

      * Comment should be just far enough from Tint to be read comfortably.

      * Comment, Text and Emphasis - used for the majority of text - should be
        equally spaced, with the distinction between them being more apparent
        than in solarized, but still not conspicuous.

      * Highlight - used sparingly to draw attention to keywords - is farthest
        from Backgrnd.

3.  Accent colors:

      * Have the same perceived lightness as Text, unlike solarized.

      * Have sufficiently low saturation (chroma) to not appear distracting.
        The accent colors are as muted as possible, unlike solarized, while
        still being recognizable even for individual words, to enable use in
        syntax highlighting of source code.

      * Are well separated in perceived hue. Given the limited color accuracy
        and gamut of commodity displays, this requires additional spacing
        between Blue and Cyan, and reduced spacing between Magenta, Violet and
        Blue.

      * Approximate the canonical hue of their namesakes. This departure from
        solarized is meant to make the colors more widely recognizable and
        reduce confusion. In particular, Green, Yellow and Orange appear closer
        to "standard" green, yellow and orange in hue, as opposed to
        yellow-green, yellow-orange and orange-red, respectively.

Note that, while solarized uses the same color values in both light and dark
palettes, doing so is explicitly *not* a requirement for Lightbox. Relaxing this
constraint is required to meet the above requirements.


# COLOR VALUES

The Lightbox palettes are based on the Munsell color system, which is
perceptually uniform unlike CIE L*a*b (see [Lindbloom], [MCSL]). Empirically
determined manual adjustments to some elements were necessary, to satisfy the
above perceptual requirements and to account for the limited precision in
response and color accuracy of commodity displays.

The adjustments are most evident in the accent colors. Uniformly spaced
Munsell hues of constant value and chroma deviate subtly from the perceptual
requirements. With these adjustments, the perceived difference between the
palette elements matches the design intent more closely.

In the table below, colors are specified by Munsell hue, value and chroma
triplets (in the standard H V/C notation) as well as their corresponding sRGB
coordinates according to [renotation data]. Note that the grayscale value and
chroma are approximate, while the accent color values are linearly interpolated.


                                  Dark palette                     Light palette
                                  -----------------------------    -----------------------------
NAME         16/8    TERMCOL      Munsell*          sRGB           Munsell*          sRGB
---------    ----    --------     --------------    -----------    --------------    -----------
highlight     8/4    brblack      5Y    9.7 /1.3    253 246 228    N     0   /0        0   0   0
focus         0/4    black        5Y    9.3 /1.3    237 230 211    10BG  2   /2       33  53  56
emphasis     10/7    brgreen      10BG  7.75/1.5    181 197 198    10BG  3.5 /2       65  89  92
text         11/7    bryellow     10BG  6.25/1.5    142 161 162    10BG  5   /2      102 128 130
note         12/6    brblue       10BG  5.5 /1.5    121 141 142    10BG  5.75/1.5    127 145 146
comment      14/4    brcyan       10BG  4.75/1.5    101 122 124    10BG  6.5 /1.3    148 164 165
tint          7/7    white        10BG  1.75/1.7     26  46  52    5Y    9.3 /1.3    237 230 211
backgrnd     15/7    brwhite      10BG  1.3 /1.7     15  37  43    5Y    9.7 /1.3    253 246 228

blue          4/4    blue         2.5PB 6.25/8      101 161 211    2.5PB 5.25/8       73 135 184
cyan          6/6    cyan         10G   6.25/5       95 169 148    10G   5.5 /7       29 153 128
green         2/2    green        7.5GY 6.5 /8      126 177  83    7.5GY 5.75/8      106 156  64
yellow        3/3    yellow       5Y    6.25/5      174 155  89    5Y    5.5 /7      160 134  39
orange        9/3    brred        2.5YR 6   /6      197 134 104    2.5YR 5.25/8      189 109  68
red           1/1    red          2.5R  6   /12     238 107 119    2.5R  5   /16     232  45  90
magenta       5/5    magenta      10P   6   /7      185 133 172    10P   5.25/8      170 109 156
violet       13/5    brmagenta    10PB  6.25/8      154 149 207    10PB  5.25/8      130 122 181


Certain aesthetic choices were made in selecting the above values.

  * The grayscale is close to - but not quite - neutral, with yellow and
    blue-green hues (of minimal chroma) at the extremes. It is noticeably more
    neutral than the solarized grayscale.

  * Red has higher chroma - and is more prominent - than the other accent
    colors. This assumes the sparing use of Red to indicate errors.


# COMPARISON TO OTHER PALETTES

## SOLARIZED

  * Solarized uses the same 16 color values for both light and dark palettes.
    This imposes certain symmetry constraints of the selection of color values.
    For terminal emulators, switching between light and dark profiles is
    typically managed separately in each terminal application. Lightbox does not
    use the same color values for both light and dark palettes, though some
    grayscale values are shared.

  * In solarized, the distinction between the comment, text and emphasis
    elements is more subtle than in Lightbox.

  * The accent colors in solarized are more saturated (higher chroma). In
    addition, the colors vary in perceived lightness (value) and overall do not
    match the lightness of text. Finally, green, yellow and orange have
    non-standard hues, while orange and red are hard to distinguish, both of
    which confuse users. Taken together, the solarized accents can divert
    attention away from the content of the text. These issues are addressed in
    Lightbox.

## BASE16

...


# SUGGESTED COLOR MAPPING

## Terminal

background            Backgrnd
foreground            Text
cursor                Highlight
emphasis foreground   Highlight
selection background  Highlight
selection foreground  Note

## diff

same    Text
old     Magenta
new     Cyan
hunk/function  Blue
header  Emphasis

## git diff

same      Text
old       Magenta
new       Cyan
hunk      Blue
function  Blue
header    Emphasis
commit    Reverse

## Syntax highlighting

Keyword   Green
Type      Yellow
Function  Blue
Variable  Text
Number    Text
String    Comment
Comment   Comment
Preproc   Violet
Special   Cyan  (escape characters, regexp, label, etc.)
Todo      Magenta
Warning   Orange
Error     Red


# References

[solarized]: http://ethanschoonover.com/solarized
[renotation data]: http://www.rit-mcsl.org/MunsellRenotation/real_sRGB.xls
[Lindbloom]: http://www.brucelindbloom.com/index.html?UPLab.html
[MCSL]: https://www.rit.edu/cos/colorscience/re_IntroducingWptLab.php
