

" ----------  Text width  ----------------------------------------------------

" Line wrapping.
set nowrap

" Mark the right margin on long lines.
autocmd BufWinEnter * execute "match Visual '\\%" . &textwidth . "v.'"
