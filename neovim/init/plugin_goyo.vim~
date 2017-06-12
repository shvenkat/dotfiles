

" ----------  goyo  ----------------------------------------------------------

let g:goyo_width = 102
let g:goyo_height = '100%'
" let g:goyo_linenr = 0
noremap <leader>d :Goyo<CR>i<Esc>`^

function! s:goyo_enter()
  " silent !tmux set status off
  " silent !tmux list-panes -F '\#F' | grep -q Z || tmux resize-pane -Z
  set noshowmode
  set noshowcmd
  let l:normal_bg = synIDattr(synIDtrans(hlID('Normal')), 'bg')
  execute "highlight! NeomakeMessageSign ctermfg=4 ctermbg=" . l:normal_bg
  execute "highlight! NeomakeInfoSign    ctermfg=2 ctermbg=" . l:normal_bg
  execute "highlight! NeomakeWarningSign ctermfg=3 ctermbg=" . l:normal_bg
  execute "highlight! NeomakeErrorSign   ctermfg=1 ctermbg=" . l:normal_bg
endfunction

function! s:goyo_leave()
  " silent !tmux set status on
  " silent !tmux list-panes -F '\#F' | grep -q Z && tmux resize-pane -Z
  set showmode
  set showcmd
  let l:sign_bg = synIDattr(synIDtrans(hlID('SignColumn')), 'bg')
  execute "highlight! NeomakeMessageSign ctermfg=4 ctermbg=" . l:sign_bg
  execute "highlight! NeomakeInfoSign    ctermfg=2 ctermbg=" . l:sign_bg
  execute "highlight! NeomakeWarningSign ctermfg=3 ctermbg=" . l:sign_bg
  execute "highlight! NeomakeErrorSign   ctermfg=1 ctermbg=" . l:sign_bg
endfunction

autocmd! User GoyoEnter nested call <SID>goyo_enter()
autocmd! User GoyoLeave nested call <SID>goyo_leave()
