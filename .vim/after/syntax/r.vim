hi link rBoolean Comment
hi link rDelimiter Comment
hi link rString Comment
hi link rNumber Comment
hi link rInteger Comment
hi link rFloat Comment

hi link rFunction Type
syn match myFunction "[a-zA-Z._]\+(\@="
hi def link myFunction Type

hi Error term=bold cterm=bold ctermfg=NONE ctermbg=1 guifg=White guibg=Red
