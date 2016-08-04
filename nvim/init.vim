" ---- PLUGIN LOADING ----

call plug#begin('~/.config/nvim/bundle')

" Basic settings
Plug 'tpope/vim-sensible'

" File/buffer navigation
Plug 'kien/ctrlp.vim'

" Look and feel
Plug 'altercation/vim-colors-solarized', { 'do': 'mkdir -p ~/.config/nvim/after/plugin && ln -s ~/.rc/nvim/after/plugin/solarized.vim ~/.config/nvim/after/plugin/' }
Plug 'vim-airline/vim-airline', { 'do': 'mkdir -p ~/.config/nvim/after/plugin && ln -s ~/.rc/nvim/after/plugin/airline.vim ~/.config/nvim/after/plugin/' }
Plug 'vim-airline/vim-airline-themes', { 'do': 'mkdir -p ~/.config/nvim/autoload/airline/themes && ln -s ~/.rc/nvim/autoload/airline/themes/lunarized.vim ~/.config/nvim/autoload/airline/themes/' }
Plug 'junegunn/goyo.vim'

" Shortcuts
Plug 'tpope/vim-commentary'
Plug 'godlygeek/tabular'
Plug 'maxbrunsfeld/vim-yankstack'
Plug 'jiangmiao/auto-pairs'

" SCM
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'

" IDE
Plug 'neomake/neomake'
Plug 'majutsushi/tagbar'
Plug 'Valloric/YouCompleteMe', { 'do': './install.py' }
Plug 'SirVer/ultisnips'
Plug 'vim-scripts/TaskList.vim'

" Lang
Plug 'tpope/vim-markdown'
Plug 'nelstrom/vim-markdown-folding'
" Plug 'vim-pandoc/vim-pandoc'
" Plug 'vim-pandoc/vim-pandoc-syntax'
"let g:pandoc#syntax#codeblocks#embeds#langs=['java', 'python']
" Plug 'jalvesaq/Nvim-R'

call plug#end()


" ---- CORE CONFIG ----

" Load vim-sensible right away, so its settings can be over-ridden.
runtime! plugin/sensible.vim

" Basic settings
set noswapfile
set hidden
set confirm
set nonumber relativenumber numberwidth=2
set hlsearch incsearch
set updatetime=1000
set wildmode=longest,list:longest
set diffopt=filler,context:3,iwhite,vertical,foldcolumn:2
set lazyredraw

" Key mapping
let mapleader=","
let maplocalleader=","
set pastetoggle=<F2>

" Whitespace
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

" Folding
set foldenable
set foldlevelstart=99
nnoremap <leader>z za
autocmd ColorScheme * highlight! link Folded Normal
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

" Syntax completion
set complete=".,w,b,u,t,i"
set completeopt="menu,menuone,longest,preview"
" inoremap <Nul> <C-x><C-o>    " C-Space invokes completion

" Opening position
" When editing a file, jump to the last known cursor position.
autocmd BufReadPost *
  \ if line("'\"") > 0 && line("'\"") <= line("$") |
  \   exe "normal g`\"" |
  \ endif

" Colorscheme
set background=dark
autocmd ColorScheme * match Visual '\%81v.'
autocmd ColorScheme * highlight! link CursorLineNr LineNr
autocmd ColorScheme * highlight! link SignColumn LineNr
autocmd ColorScheme * highlight Todo term=reverse cterm=reverse ctermfg=5
" Retrieves the color for a provided scope and swatch in the current context
function! LoadColor(scope, swatch)
  let l:scopeColor = synIDattr(synIDtrans(hlID(a:scope)), a:swatch)
  return l:scopeColor < 0 ? 'none' : l:scopeColor
endfunction

" Python provider
let g:python_host_prog='python3'


" ---- PLUGIN CONFIG ----

" ctrlp
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlPBuffer'

" gitgutter
let g:gitgutter_enabled = 1
let g:gitgutter_sign_column_always = 1
let g:gitgutter_escape_grep = 1
" let g:gitgutter_diff_args = '-b'
let g:gitgutter_realtime = 1
let g:gitgutter_eager = 1
let g:gitgutter_override_sign_column_highlight = 0
let g:gitgutter_sign_added = '+'
let g:gitgutter_sign_removed = '_'
let g:gitgutter_sign_modified = '±'
let g:gitgutter_sign_modified_removed = '±'
highlight! link GitGutterAdd SignColumn
highlight! link GitGutterDelete SignColumn
highlight! link GitGutterChange SignColumn
highlight! link GitGutterChangeDelete SignColumn

" vim-airline
let g:airline_theme = 'lunarized'
let g:airline#extensions#default#layout = [
      \ [ 'b', 'c', 'x' ],
      \ [ 'error', 'warning', 'z', 'a', 'y' ]
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

" goyo
let g:goyo_width = 90
let g:goyo_height = '100%'
" let g:goyo_linenr = 0
noremap <leader>d :Goyo<CR>i<Esc>`^

" vim-yankstack
nmap <leader>p <Plug>yankstack_substitute_older_paste
nmap <leader>P <Plug>yankstack_substitute_newer_paste

" neomake
let g:neomake_python_enabled_makers = ['mypy', 'flake8']
let g:neomake_ft_maker_remove_invalid_entries = 0
let g:neomake_error_sign = {'text': 'E', 'texthl': 'NeomakeErrorSign'}
let g:neomake_warning_sign = {'text': 'W', 'texthl': 'NeomakeWarningSign'}
let g:neomake_message_sign = {'text': 'M', 'texthl': 'NeomakeMessageSign'}
let g:neomake_info_sign = {'text': 'I', 'texthl': 'NeomakeInfoSign'}
autocmd BufWritePost,BufEnter * Neomake
function! SolarizeNeomakeColors()
  let l:sign_bg = LoadColor('SignColumn', 'bg')
  execute "highlight! NeomakeMessageSign ctermfg=4 ctermbg=" . l:sign_bg
  execute "highlight! NeomakeInfoSign    ctermfg=2 ctermbg=" . l:sign_bg
  execute "highlight! NeomakeWarningSign ctermfg=3 ctermbg=" . l:sign_bg
  execute "highlight! NeomakeErrorSign   ctermfg=1 ctermbg=" . l:sign_bg
endfunction
autocmd ColorScheme * call SolarizeNeomakeColors()

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
autocmd! User YouCompleteMe if !has('vim_starting') | call youcompleteme#Enable() | endif

" ultisnips
let g:UltiSnipsExpandTrigger = "<leader>se"
let g:UltiSnipsListSnippets = "<leader>sl"
let g:UltiSnipsJumpForwardTrigger = "<Tab>"
let g:UltiSnipsJumpBackwardTrigger = "<S-Tab>"
let g:UltiSnipsSnippetsDir = "~/.vim/snips/ultisnips"
let g:UltiSnipsSnippetDirectories = ["UltiSnips", "snips/ultisnips"]

" vim-markdown
let g:markdown_fold_style='nested'
