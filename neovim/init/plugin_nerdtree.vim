

" ----------  NERDTree  ------------------------------------------------------

map <C-n> :NERDTreeToggle<CR>

" Show hidden files.
let NERDTreeShowHidden=1

" Ignore certain files and directories.
let NERDTreeIgnore=[
    \ '^\.git$[[dir]]',
    \ '^env$[[dir]]',
    \ '^\.venv$[[dir]]',
    \ '^dist$[[dir]]',
    \ '\.egg-info$[[dir]]',
    \ '^__pycache__$[[dir]]',
    \ '^\.mypy_cache$[[dir]]',
    \ '^\.hypothesis$[[dir]]',
    \ ]
