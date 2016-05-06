let g:solarized_termcolors = 16
let g:solarized_termtrans = 0
let g:solarized_degrade = 0
let g:solarized_bold = 0
let g:solarized_underline = 0
let g:solarized_italic = 0
let g:solarized_contrast = "normal"
let g:solarized_visibility = "normal"

set background=dark
colorscheme solarized

" hi Folded term=NONE cterm=NONE gui=NONE
hi! link Folded Normal
hi Todo term=reverse cterm=reverse ctermfg=5
