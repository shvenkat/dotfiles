

" ----------  gitgutter  -----------------------------------------------------

let g:gitgutter_enabled = 1
let g:gitgutter_sign_column_always = 1
let g:gitgutter_realtime = 1
let g:gitgutter_eager = 1

let g:gitgutter_escape_grep = 1
" let g:gitgutter_diff_args = '-b'

let g:gitgutter_sign_added = '+'
let g:gitgutter_sign_removed = '_'
let g:gitgutter_sign_modified = '±'
let g:gitgutter_sign_modified_removed = '±'

let g:gitgutter_override_sign_column_highlight = 0
highlight! link GitGutterAdd SignColumn
highlight! link GitGutterDelete SignColumn
highlight! link GitGutterChange SignColumn
highlight! link GitGutterChangeDelete SignColumn
