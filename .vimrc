" ---- PACKAGE MANAGEMENT ----------------------------------------------------

filetype off

" Call pathogen to configure extension loading. Extensions are loaded AFTER
" .vimrc is read.
call pathogen#infect("bundle.snappy/{}")
call pathogen#helptags()

" Load vim-sensible right away, so its options can be over-ridden.
runtime! plugin/sensible.vim

" ---- VIM CORE CONFIG -------------------------------------------------------

" Basic options
set nocompatible
set noswapfile
set hidden
set confirm
set number relativenumber
set hlsearch incsearch
set updatetime=1000
set wildmode=longest,list:longest
set diffopt=filler,context:3,iwhite,vertical,foldcolumn:2
set lazyredraw

" Generic key mapping
let mapleader=","
let maplocalleader=","
set pastetoggle=<F2>

" Whitespace management
set tabstop=8                   " A tab is 8 spaces
set expandtab                   " Always uses spaces instead of tabs
set softtabstop=4               " Insert 4 spaces when tab is pressed
set shiftwidth=4                " An indent is 4 spaces
set smarttab                    " Indent instead of tab at start of line
set shiftround                  " Round spaces to nearest shiftwidth multiple
set nojoinspaces                " Don't convert spaces to tabs
set autoindent
set nowrap
" Remove trailing whitespace.
noremap <leader>w :%s/\s\+$//e<CR>

" Line width
match Visual '\%81v.'
" set textwidth=79
" if exists('+colorcolumn')
"   set colorcolumn=80  " A right gutter to balance the left one.
" else
"   autocmd BufWinEnter * let w:m1=matchadd('ColorColumn', '\%>79v.\+', -1)
" endif

" Left margin
" autocmd BufEnter * :normal m>    "showMarks complains if there are no marks
autocmd ColorScheme * highlight! link SignColumn LineNr
autocmd ColorScheme * highlight! link CursorLineNr SignColumn

" Folding
" Returns the text of the first line of a folded block, with ellipses.
" Modified from the original at https://gist.github.com/sjl/3360978.
function! MyFoldText()
  let ellipses = ' ...'
  " Calculate foldtext width, accounting for fold, sign and number columns.
  let marginwidth = &fdc + 2 + &number * &numberwidth
  let foldtextwidth = winwidth(0) - marginwidth - len(ellipses)
  " Format foldtext by expanding tabs, adding ellipses and truncating/padding.
  let line = getline(v:foldstart)
  let line = substitute(line, '\t', repeat(' ', &softtabstop), 'g')
  let line = strpart(line, 0, foldtextwidth)
  let line = line . ellipses . repeat(' ', foldtextwidth - len(line))
  return line
endfunction
set foldtext=MyFoldText()
set foldenable
set foldlevelstart=99
nnoremap <leader>z za
" vnoremap <Space> za
" Refocus folds on current line
" nnoremap <leader>z zMzvzz
" Open current top-level fold
" nnoremap zO zCzO

" Syntax completion
set complete=".,w,b,u,t,i"
set completeopt="menu,menuone,longest,preview"
" set omnifunc=syntaxcomplete#Complete
" inoremap <leader>, <C-x><C-o>
" inoremap <Nul> <C-x><C-o>    " C-Space invokes completion

" Opening position
" When editing a file, jump to the last known cursor position.
autocmd BufReadPost *
  \ if line("'\"") > 0 && line("'\"") <= line("$") |
  \   exe "normal g`\"" |
  \ endif

" Colorscheme
set background=dark
autocmd ColorScheme * highlight! link Folded Normal
autocmd ColorScheme * highlight Todo term=reverse cterm=reverse ctermfg=5
" Retrieves the color for a provided scope and swatch in the current context
function! LoadColor(scope, swatch)
  let l:scopeColor = synIDattr(synIDtrans(hlID(a:scope)), a:swatch)
  return l:scopeColor < 0 ? 'none' : l:scopeColor
endfunction

" ---- PLUGIN CONFIG ---------------------------------------------------------

" ctrlp
autocmd BufEnter * noremap <C-p> :CtrlPBuffer<CR>

" nerdtree
autocmd BufEnter * noremap <C-n> :NERDTreeToggle<CR>

" gitgutter
let g:gitgutter_enabled = 1
let g:gitgutter_sign_column_always = 1
let g:gitgutter_escape_grep = 1
" let g:gitgutter_diff_args = '-b'
let g:gitgutter_realtime = 1
let g:gitgutter_eager = 1
" let g:gitgutter_override_sign_column_highlight = 0
function! CustomizeGitGutterColors()
  let l:sign_bg = LoadColor('SignColumn', 'bg')
  execute "highlight GitGutterAdd          ctermfg=6 ctermbg=" . l:sign_bg
  execute "highlight GitGutterDelete       ctermfg=5 ctermbg=" . l:sign_bg
  execute "highlight GitGutterChange       ctermfg=3 ctermbg=" . l:sign_bg
  execute "highlight GitGutterChangeDelete ctermfg=3 ctermbg=" . l:sign_bg
endfunction

" vim-colors-solarized
function! Solarize()
  colorscheme solarized
  call CustomizeGitGutterColors()
  LiteDFMToggle
  LiteDFMToggle
endfunction
noremap <leader>cl :set background=light<CR> :call Solarize()<CR>
noremap <leader>cd :set background=dark<CR> :call Solarize()<CR>

" vim-airline
let g:airline_theme = 'lunarized'
let g:airline#extensions#default#layout = [
      \ [ 'b', 'c', 'x' ],
      \ [ 'warning', 'z', 'a', 'y' ]
      \ ]
let g:airline#extensions#default#section_truncate_width = {
    \ 'b': 60, 'z': 45, 'a': 60, 'y': 60 }
let g:airline_left_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_sep = ''
let g:airline_right_alt_sep = ''
let g:airline_detect_modified = 1
let g:airline_detect_paste = 1
let g:airline_detect_iminsert = 0
let g:airline_inactive_collapse = 0
let g:airline_powerline_fonts = 0
if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif
let g:airline_symbols.branch = ''
let g:airline_symbols.linenr = ''
let g:airline_symbols.paste = 'P'
let g:airline_symbols.space = ' '
let g:airline_mode_map = {
    \ '__' : '-',
    \ 'n'  : 'N',
    \ 'i'  : 'I',
    \ 'R'  : 'R',
    \ 'c'  : 'C',
    \ 'v'  : 'V',
    \ 'V'  : 'V',
    \ '^V' : 'B',
    \ 's'  : 'S',
    \ 'S'  : 'S',
    \ '^S' : 'S',
    \ }
let g:airline_exclude_preview = 0
let g:airline#extensions#hunks#enabled = 0
let g:airline#extensions#branch#enabled = 1
let g:airline#extensions#tagbar#flags = ''
let g:airline#extensions#virtualenv#enabled = 0
let g:airline#extensions#whitespace#enabled = 1
let g:airline#extensions#whitespace#mixed_indent_algo = 1
let g:airline#extensions#whitespace#symbol = '!'
let g:airline#extensions#whitespace#show_message = 1
let g:airline#extensions#tabline#enabled = 0
let g:airline#extensions#tabline#show_buffers = 0
let g:airline#extensions#tabline#show_tab_nr = 1
let g:airline#extensions#tabline#tab_nr_type = 1
let g:airline#extensions#tabline#show_tab_type = 1
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'
let g:airline#extensions#ctrlp#show_adjacent_modes = 0

" lite-dfm
noremap <leader>d :LiteDFMToggle<CR>i<Esc>`^
let g:lite_dfm_keep_statusline = 1
let g:lite_dfm_keep_gitgutter = 1

" vim-yankstack
nmap <leader>p <Plug>yankstack_substitute_older_paste
nmap <leader>P <Plug>yankstack_substitute_newer_paste

" screen
let g:ScreenImpl = 'Tmux'
let g:ScreenShellHeight = 10
let g:ScreenShellWidth = 57
let g:ScreenShellQuitOnVimExit = 1
let g:ScreenShellInitialFocus = 'vim'
let g:ScreenShellExpandTabs = 0

" tagbar
" let g:tagbar_ctags_bin = '/usr/local/bin/ctags'
let g:tagbar_sort = 0
let g:tagbar_left = 0
let g:tagbar_width = 30
let g:tagbar_compact = 1
let g:tagbar_indent = 1
let g:tagbar_show_visibility = 1
let g:tagbar_foldlevel = 2
let g:tagbar_iconchars = ['+', '-']
let g:tagbar_autoshowtag = 0
let g:tagbar_autofocus = 1
let g:tagbar_autoclose = 1
nnoremap <silent> <leader>g :TagbarToggle<CR>

" youcompleteme
let g:ycm_auto_trigger = 1
let g:ycm_key_invoke_completion = '<leader>,'
let g:ycm_key_list_select_completion = ['<C-n>']
let g:ycm_key_list_previous_completion = ['<C-p>']
let g:ycm_min_num_of_chars_for_completion = 2
let g:ycm_min_num_identifier_candidate_chars = 0
let g:ycm_add_preview_to_completeopt = 1
let g:ycm_autoclose_preview_window_after_completion = 0
let g:ycm_autoclose_preview_window_after_insertion = 0
let g:ycm_cache_omnifunc = 1

" ultisnips
let g:UltiSnipsExpandTrigger = "<leader>se"
let g:UltiSnipsListSnippets = "<leader>sl"
let g:UltiSnipsJumpForwardTrigger = "<Tab>"
let g:UltiSnipsJumpBackwardTrigger = "<S-Tab>"
let g:UltiSnipsSnippetsDir = "~/.vim/snips/ultisnips"
let g:UltiSnipsSnippetDirectories = ["UltiSnips", "snips/ultisnips"]

" syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
let g:syntastic_always_populate_loc_list = 1
" let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 1
let g:syntastic_python_pylint_args = "--rcfile .pylintrc"
let g:syntastic_python_flake8_args = "--config .flake8rc"

" showMarks
let g:showmarks_enable = 0
let g:showmarks_include = "abcdefghijklmnopqrstuvwxyz>"
autocmd ColorScheme * highlight! link ShowMarksHL SignColumn

" ack
if executable('ag')
  let g:ackprg = 'ag --vimgrep'
endif

" color_coded
let g:color_coded_enabled = 1
let g:color_coded_filetypes = ['c', 'cpp', 'objc']

" python-mode
let g:pymode_rope = 0
let g:pymode_rope_completion = 0
let g:pymode_lint = 0
" simpylfold
let g:SimpylFold_docstring_preview = 1
let g:SimpylFold_fold_docstring = 1
" autocmd BufWinEnter *.py setlocal foldexpr=SimpylFold(v:lnum) foldmethod=expr
" autocmd BufWinLeave *.py setlocal foldexpr< foldmethod<


" vim-markdown
let g:markdown_fold_style='nested'

" r-plugin
call pathogen#surround('~/.vim/bundle/r-runtime') " load runtime before plugin
let r_indent_align_args = 1
let r_syntax_folding = 1
let vimrplugin_show_args = 1
let vimrplugin_args_in_stline = 0
let vimrplugin_assign = 0
let vimrplugin_listmethods = 1
let vimrplugin_specialplot = 1
let vimrplugin_vsplit = 0    " For vertical tmux split
let vimrplugin_rconsole_height = 10
" let g:vimrplugin_screenplugin = 1  " Integrate r-plugin with screen.vim
let vimrplugin_r_args = "--interactive --quiet"
let vimrplugin_map_r = 1
imap <leader>. <Plug>RCompleteArgs
