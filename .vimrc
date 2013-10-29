" call pathogen to configure extension loading
" extensions are loaded AFTER .vimrc is read
execute pathogen#infect()

" load vim-sensible right away, so its options can be over-ridden
runtime! plugin/sensible.vim

" basic options
filetype plugin indent on
syntax on
set nowrap
set autoindent
set hidden
set confirm
set wildchar=<Tab> wildmenu wildmode=full

" vim-colors-solarized
func! SetColorschemeSolarizedLight()
  set background=light
  colorscheme solarized
endfunc
noremap <leader>l :call SetColorschemeSolarizedLight()<CR>
func! SetColorschemeSolarizedDark()
  set background=dark
  colorscheme solarized
endfunc
noremap <leader>d :call SetColorschemeSolarizedDark()<CR>
set background=dark
colorscheme solarized

" vim-markdown
"let g:vim_markdown_folding_disabled=0
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
let g:airline_powerline_fonts = 1
let g:airline#extensions#branch#enabled = 1
let g:airline#extensions#whitespace#enabled = 1
let g:airline#extensions#whitespace#show_message = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'
let g:airline#extensions#default#section_truncate_width = {
    \ 'b': 60, 'x': 60, 'y': 70, 'z': 45 }

" whitespace management
set tabstop=8                   "A tab is 8 spaces
set expandtab                   "Always uses spaces instead of tabs
set softtabstop=4               "Insert 4 spaces when tab is pressed
set shiftwidth=4                "An indent is 4 spaces
set smarttab                    "Indent instead of tab at start of line
set shiftround                  "Round spaces to nearest shiftwidth multiple
set nojoinspaces                "Don't convert spaces to tabs
func! DeleteTrailingWS()
  exe "normal mz"
  %s/\s\+$//e
  exe "normal `z"
endfunc
noremap <leader>w :call DeleteTrailingWS()<CR>

" line width
"set textwidth=79
if exists('+colorcolumn')
  set colorcolumn=80
else
  autocmd BufWinEnter * let w:m1=matchadd('ColorColumn', '\%>79v.\+', -1)
endif
