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
"set foldmethod=indent
set hidden
set confirm
set wildchar=<Tab> wildmenu wildmode=full
"set backspace=indent,eol,start
"set number
"set ruler
set noswapfile
set hlsearch

" When editing a file, jump to the last known cursor position
autocmd BufReadPost *
  \ if line("'\"") > 0 && line("'\"") <= line("$") |
  \   exe "normal g`\"" |
  \ endif

let mapleader=","
set pastetoggle=<F2>
"map Q gq " Don't use Ex mode, use Q for formatting
map <F10> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
\ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
\ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>

set omnifunc=syntaxcomplete#Complete
inoremap <leader>, <C-x><C-o>
"inoremap <Nul> <C-x><C-o>
"if has("gui_running")
"    " C-Space seems to work under gVim on both Linux and win32
"    inoremap <C-Space> <C-n>
"else " no gui
"  if has("unix")
"    inoremap <Nul> <C-n>
"  else
"  " I have no idea of the name of Ctrl-Space elsewhere
"  endif
"endif

" whitespace management
set tabstop=8                   "A tab is 8 spaces
set expandtab                   "Always uses spaces instead of tabs
set softtabstop=4               "Insert 4 spaces when tab is pressed
set shiftwidth=4                "An indent is 4 spaces
set smarttab                    "Indent instead of tab at start of line
set shiftround                  "Round spaces to nearest shiftwidth multiple
set nojoinspaces                "Don't convert spaces to tabs
func! DeleteTrailingWS()
  exe "normal mp"
  %s/\s\+$//e
  exe "normal `p"
  delmarks p
endfunc
noremap <leader>w :call DeleteTrailingWS()<CR>

" line width
"set textwidth=79
if exists('+colorcolumn')
  set colorcolumn=80
else
  autocmd BufWinEnter * let w:m1=matchadd('ColorColumn', '\%>79v.\+', -1)
endif

" sign column
autocmd BufEnter * :normal m>
autocmd ColorScheme * highlight! link SignColumn LineNr

" nerdtree
map <C-n> :NERDTreeToggle<CR>

" vim-colors-solarized
func! SetColorschemeSolarizedLight()
  set background=light
  colorscheme solarized
  hi Folded term=NONE cterm=NONE gui=NONE
endfunc
noremap <leader>sl :call SetColorschemeSolarizedLight()<CR>
func! SetColorschemeSolarizedDark()
  set background=dark
  colorscheme solarized
  hi Folded term=NONE cterm=NONE gui=NONE
endfunc
noremap <leader>sd :call SetColorschemeSolarizedDark()<CR>
set background=dark
colorscheme solarized

" vim-markdown
" vim-markdown-folding
"let g:markdown_fold_style='nested'

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

" screen
"let g:ScreenImpl = 'Tmux'
"let g:ScreenShellHeight = 15
"let g:ScreenShellWidth = -1
"let g:ScreenShellQuitOnVimExit = 1
"let g:ScreenShellInitialFocus = 'vim'
"let g:ScreenShellAttachTargetCurrent = 0
"let g:ScreenShellExpandTabs = 0

" showMarks
"let g:showmarks_enable = 0
let g:showmarks_include = "abcdefghijklmnopqrstuvwxyz>"

" vim-r-plugin
let r_indent_align_args = 0
let r_syntax_folding = 1
let vimrplugin_assign = 0
"let vimrplugin_screenvsplit = 1    " For vertical tmux split
"let g:vimrplugin_screenplugin = 1  " Integrate r-plugin with screen.vim
"vmap <Space> <Plug>RDSendSelection
"nmap <Space> <Plug>RDSendLine
let vimrplugin_r_args = "--interactive"

" Folding
set foldenable
" All folds closed by default
set foldlevelstart=0
" Space to toggle folds
nnoremap <Space> za
vnoremap <Space> za
" Refocus folds on current line
nnoremap <leader>z zMzvzz
" Open current top-level fold
nnoremap zO zCzO
" From https://gist.github.com/sjl/3360978
function! MyFoldText() " {{{
    let line = getline(v:foldstart)
 
    let nucolwidth = &fdc + &number * &numberwidth
    let windowwidth = winwidth(0) - nucolwidth - 3
    let foldedlinecount = v:foldend - v:foldstart
 
    " expand tabs into spaces
    let onetab = strpart('          ', 0, &tabstop)
    let line = substitute(line, '\t', onetab, 'g')
 
    let line = strpart(line, 0, windowwidth - 6 -len(foldedlinecount))
    let fillcharcount = windowwidth - 4 - len(line) - len(foldedlinecount)
    return line . '...' . repeat(" ",fillcharcount) . '(' . foldedlinecount . ')'
endfunction " }}}
set foldtext=MyFoldText()
