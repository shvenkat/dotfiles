" ---- PACKAGE MANAGEMENT ----------------------------------------------------

" call pathogen to configure extension loading
" extensions are loaded AFTER .vimrc is read
execute pathogen#infect()

" load vim-sensible right away, so its options can be over-ridden
runtime! plugin/sensible.vim

" ---- VIM CORE CONFIG -------------------------------------------------------

" basic options
set nocompatible
set noswapfile
set hidden
set confirm
set number
set hlsearch incsearch
set updatetime=1000
"set wildchar=<Tab> wildmenu wildmode=full

" generic key mapping
let mapleader=","
set pastetoggle=<F2>

" whitespace management
set tabstop=8                   "A tab is 8 spaces
set expandtab                   "Always uses spaces instead of tabs
set softtabstop=4               "Insert 4 spaces when tab is pressed
set shiftwidth=4                "An indent is 4 spaces
set smarttab                    "Indent instead of tab at start of line
set shiftround                  "Round spaces to nearest shiftwidth multiple
set nojoinspaces                "Don't convert spaces to tabs
set nowrap
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
"autocmd BufEnter * :normal m>
autocmd ColorScheme * highlight! link SignColumn LineNr

" folding
set foldenable
set foldlevelstart=99
nnoremap <Space> za
vnoremap <Space> za
nnoremap <leader>z zMzvzz     " Refocus folds on current line
nnoremap zO zCzO              " Open current top-level fold
" From https://gist.github.com/sjl/3360978
function! MyFoldText() " {{{
    let line = getline(v:foldstart)

    let nucolwidth = &fdc + &number * &numberwidth
    let windowwidth = winwidth(0) - nucolwidth - 3
    let foldedlinecount = v:foldend - v:foldstart

    " expand tabs into spaces
    let onetab = strpart('          ', 0, &tabstop)
    let line = substitute(line, '\t', onetab, 'g')

    let line = strpart(line, 0, windowwidth - 4 -len(foldedlinecount))
    let fillcharcount = windowwidth - 2 - len(line) - len(foldedlinecount)
    return line . '...' . repeat(" ",fillcharcount) . '' . foldedlinecount . ''
endfunction " }}}
set foldtext=MyFoldText()

" syntax completion
set complete=".,w,b,u,t,i"
set omnifunc=syntaxcomplete#Complete
inoremap <leader>, <C-x><C-o>
"inoremap <Nul> <C-x><C-o>    " C-Space invokes completion
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

" When editing a file, jump to the last known cursor position
autocmd BufReadPost *
  \ if line("'\"") > 0 && line("'\"") <= line("$") |
  \   exe "normal g`\"" |
  \ endif

" ---- PLUGIN CONFIG ---------------------------------------------------------

" nerdtree
map <C-n> :NERDTreeToggle<CR>

" vim-colors-solarized
func! SetColorschemeSolarizedLight()
  " let g:solarized_termcolors = 16
  set background=light
  colorscheme solarized
  hi Folded term=NONE cterm=NONE gui=NONE
  hi Todo term=reverse cterm=reverse ctermfg=5
endfunc
noremap <leader>sl :call SetColorschemeSolarizedLight()<CR>
func! SetColorschemeSolarizedDark()
  " let g:solarized_termcolors = 16
  set background=dark
  colorscheme solarized
  hi Folded term=NONE cterm=NONE gui=NONE
  hi Todo term=reverse cterm=reverse ctermfg=5
endfunc
noremap <leader>sd :call SetColorschemeSolarizedDark()<CR>

" vim-markdown
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

" gitgutter
let g:gitgutter_enabled = 1
let g:gitgutter_sign_column_always = 1
let g:gitgutter_diff_args = '-b'
let g:gitgutter_realtime = 1
let g:gitgutter_eager = 1

" vim-airline
let g:airline_powerline_fonts = 1
let g:airline#extensions#branch#enabled = 1
let g:airline#extensions#whitespace#enabled = 1
let g:airline#extensions#whitespace#show_message = 1
let g:airline#extensions#tabline#enabled = 0
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
let r_indent_align_args = 1
let r_syntax_folding = 1
let vimrplugin_assign = 0
let vimrplugin_vsplit = 0    " For vertical tmux split
let vimrplugin_rconsole_height = 10
"let g:vimrplugin_screenplugin = 1  " Integrate r-plugin with screen.vim
"vmap <Space> <Plug>RDSendSelection
"nmap <Space> <Plug>RDSendLine
let vimrplugin_r_args = "--interactive --quiet"
imap <leader>. <Plug>RCompleteArgs
