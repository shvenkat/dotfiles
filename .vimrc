" ---- PACKAGE MANAGEMENT ----------------------------------------------------

filetype off

" call pathogen to configure extension loading
" extensions are loaded AFTER .vimrc is read
call pathogen#infect()
call pathogen#helptags()

" load vim-sensible right away, so its options can be over-ridden
runtime! plugin/sensible.vim

" ---- VIM CORE CONFIG -------------------------------------------------------

" basic options
set nocompatible
set noswapfile
set hidden
set confirm
set number relativenumber
set hlsearch incsearch
set updatetime=1000
set wildmode=longest,list:longest
set diffopt=filler,context:3,iwhite,vertical,foldcolumn:2

" generic key mapping
let mapleader=","
let maplocalleader=","
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
noremap <leader>w :%s/\s\+$//e<CR>    "Remove trailing whitespace

" line width
"set textwidth=79
if exists('+colorcolumn')
  set colorcolumn=80
else
  autocmd BufWinEnter * let w:m1=matchadd('ColorColumn', '\%>79v.\+', -1)
endif

" left column
" autocmd BufEnter * :normal m>    "showMarks complains if there are no marks
autocmd ColorScheme * highlight! link SignColumn LineNr
autocmd ColorScheme * highlight! link CursorLineNr SignColumn

" folding
set foldenable
set foldlevelstart=99
nnoremap <Space> za
vnoremap <Space> za
nnoremap <leader>z zMzvzz     "Refocus folds on current line
nnoremap zO zCzO              "Open current top-level fold
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
set completeopt="menu,menuone,longest,preview"
" set omnifunc=syntaxcomplete#Complete
" inoremap <leader>, <C-x><C-o>
" inoremap <Nul> <C-x><C-o>    " C-Space invokes completion

" When editing a file, jump to the last known cursor position
autocmd BufReadPost *
  \ if line("'\"") > 0 && line("'\"") <= line("$") |
  \   exe "normal g`\"" |
  \ endif

" Colorscheme tweaks
autocmd ColorScheme * highlight Folded term=NONE cterm=NONE gui=NONE
autocmd ColorScheme * highlight Todo term=reverse cterm=reverse ctermfg=5

" ---- PLUGIN CONFIG ---------------------------------------------------------

" ctrlp
autocmd BufEnter * noremap <C-p> :CtrlPBuffer<CR>

" nerdtree
autocmd BufEnter * noremap <C-n> :NERDTreeToggle<CR>

" gitgutter
let g:gitgutter_enabled = 1
let g:gitgutter_sign_column_always = 1
let g:gitgutter_escape_grep = 1
let g:gitgutter_diff_args = '-b'
let g:gitgutter_realtime = 1
let g:gitgutter_eager = 1

" vim-colors-solarized
noremap <leader>csl :set background=light<CR> :colorscheme solarized<CR>
noremap <leader>csd :set background=dark<CR> :colorscheme solarized<CR>

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

" showMarks
let g:showmarks_enable = 0
let g:showmarks_include = "abcdefghijklmnopqrstuvwxyz>"
autocmd ColorScheme * highlight! link ShowMarksHL SignColumn

" ack
if executable('ag')
  let g:ackprg = 'ag --vimgrep'
endif

" python-mode
let g:pymode_rope = 0
let g:pymode_rope_completion = 0
let g:pymode_lint = 0

" vim-markdown
let g:markdown_fold_style='nested'    "alternative is 'nested'

" r-plugin
call pathogen#surround('~/.vim/bundle/r-runtime') "load runtime before plugin
let r_indent_align_args = 1
let r_syntax_folding = 1
let vimrplugin_show_args = 1
let vimrplugin_args_in_stline = 0
let vimrplugin_assign = 0
let vimrplugin_listmethods = 1
let vimrplugin_specialplot = 1
let vimrplugin_vsplit = 0    " For vertical tmux split
let vimrplugin_rconsole_height = 10
"let g:vimrplugin_screenplugin = 1  " Integrate r-plugin with screen.vim
let vimrplugin_r_args = "--interactive --quiet"
let vimrplugin_map_r = 1
imap <leader>. <Plug>RCompleteArgs
