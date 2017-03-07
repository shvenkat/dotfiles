

" ----------  Misc settings  -------------------------------------------------

" Line numbers.
set nonumber relativenumber numberwidth=2

" Screen updates.
set lazyredraw
set scrolloff=3

" Search.
set hlsearch incsearch

" Command completion.
set wildmode=longest,list:longest

" Diff mode.
set diffopt=filler,context:3,iwhite,vertical,foldcolumn:2

" Unsaved changes.
set noswapfile    " Avoid saving sensitive information in the clear.
set hidden
set confirm

" When opening a file, jump to the last known cursor position.
autocmd BufReadPost *
  \ if line("'\"") > 0 && line("'\"") <= line("$") |
  \   exe "normal g`\"" |
  \ endif

" Python for plugins.
let g:python_host_prog='python3'
