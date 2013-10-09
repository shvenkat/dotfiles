execute pathogen#infect()
runtime! plugin/sensible.vim
syntax on
filetype plugin indent on
set nowrap
set autoindent
"set textwidth=79
set hidden
set confirm
set wildchar=<Tab> wildmenu wildmode=full
" vim-colors-solarized
set background=light
colorscheme solarized
" vim-markdown
let g:vim_markdown_folding_disabled=1
let g:vim_markdown_initial_foldlevel=1
" changesPlugin
let g:changes_hl_lines=0
let g:changes_autocmd=1
let g:changes_verbose=0
"hi DiffAdd    term=bold ctermbg=4 guibg=DarkBlue
"hi DiffDelete term=bold ctermbg=4 guibg=DarkBlue
"hi DiffChange term=bold ctermbg=4 guibg=DarkBlue
"let g:changes_vcs_check=1
"let g:changes_vcs_system='git'
" vim-airline
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'
let g:airline_powerline_fonts = 1
