

" ----------  neomake  -------------------------------------------------------

let g:neomake_ft_maker_remove_invalid_entries = 0
let g:neomake_python_enabled_makers = ['flake8', 'mypy']
let g:neomake_sh_enabled_makers = ['sh', 'shellcheck']

autocmd BufWritePost,BufEnter * Neomake

let g:neomake_error_sign = {'text': 'E', 'texthl': 'NeomakeErrorSign'}
let g:neomake_warning_sign = {'text': 'W', 'texthl': 'NeomakeWarningSign'}
let g:neomake_message_sign = {'text': 'M', 'texthl': 'NeomakeMessageSign'}
let g:neomake_info_sign = {'text': 'I', 'texthl': 'NeomakeInfoSign'}

function! SolarizeNeomakeColors()
  let l:sign_bg = synIDattr(synIDtrans(hlID('SignColumn')), 'bg')
  execute "highlight! NeomakeMessageSign ctermfg=4 ctermbg=" . l:sign_bg
  execute "highlight! NeomakeInfoSign    ctermfg=2 ctermbg=" . l:sign_bg
  execute "highlight! NeomakeWarningSign ctermfg=3 ctermbg=" . l:sign_bg
  execute "highlight! NeomakeErrorSign   ctermfg=1 ctermbg=" . l:sign_bg
endfunction
autocmd ColorScheme * call SolarizeNeomakeColors()
