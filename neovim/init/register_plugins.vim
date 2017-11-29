

" ----------  Plugin Selection  ----------------------------------------------

call plug#begin('~/.config/nvim/bundle')

" Basic settings
Plug 'tpope/vim-sensible'
Plug 'editorconfig/editorconfig-vim'

" File/buffer navigation
Plug 'kien/ctrlp.vim'
Plug 'scrooloose/nerdtree'

" Look and feel
Plug 'altercation/vim-colors-solarized'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
" Plug 'junegunn/goyo.vim'
" Plug 'vim-scripts/ShowMarks'

" Shortcuts
Plug 'tpope/vim-commentary'
Plug 'godlygeek/tabular'
Plug 'maxbrunsfeld/vim-yankstack'
Plug 'jiangmiao/auto-pairs'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-repeat'

" SCM
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'

" IDE
Plug 'neomake/neomake'
Plug 'majutsushi/tagbar'
" Plug 'Valloric/YouCompleteMe', { 'do': './install.py' }
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'vim-scripts/TaskList.vim'

" Lang
Plug 'tpope/vim-markdown'
Plug 'nelstrom/vim-markdown-folding'
" Plug 'vim-pandoc/vim-pandoc'
" Plug 'vim-pandoc/vim-pandoc-syntax'
" let g:pandoc#syntax#codeblocks#embeds#langs=['java', 'python']
Plug 'shvenkat/python_ifold'
" Plug 'jalvesaq/Nvim-R'
Plug 'rust-lang/rust.vim'

call plug#end()

" Load vim-sensible right away, so its settings can be over-ridden.
runtime! plugin/sensible.vim
