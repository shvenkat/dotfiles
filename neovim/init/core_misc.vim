

" ----------  Misc settings  -------------------------------------------------

" Unsaved changes.
set hidden
set confirm

" Search.
set hlsearch incsearch

" Diff mode.
set diffopt=filler,context:3,iwhite,vertical,foldcolumn:2

" Command completion.
set wildmode=longest,list:longest

" When opening a file, jump to the last known cursor position.
autocmd BufReadPost *
  \ if line("'\"") > 0 && line("'\"") <= line("$") |
  \   exe "normal g`\"" |
  \ endif

" Screen updates.
set lazyredraw
set scrolloff=3

" Python for plugins.
let g:python_host_prog='python3'
