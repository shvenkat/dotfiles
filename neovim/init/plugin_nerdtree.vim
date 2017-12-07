

" ----------  NERDTree  ------------------------------------------------------

map <C-n> :NERDTreeToggle<CR>

" Show hidden files.
let NERDTreeShowHidden=1

" Ignore certain files and directories.
let NERDTreeIgnore=[
    \ '^\.git$[[dir]]',
    \ '^\.mypy_cache$[[dir]]',
    \ '^\env$[[dir]]',
    \ '^\.venv$[[dir]]',
    \ '\.egg-info$[[dir]]',
    \ '^__pycache__$[[dir]]',
    \ ]
