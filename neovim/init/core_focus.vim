

" ----------  Simple Focus Mode  ---------------------------------------------

let g:focus_toggle_key = "<leader>f"  " Key binding.
let s:focus_state = "off"            " Tracks state transitions.

" Modify colorscheme to make certain elements less distracting.
function! s:patch_colorscheme()
    " Make the left margin less conspicuous.
    let l:margin_fg = synIDattr(synIDtrans(hlID('LineNr')), 'bg')
    let l:margin_bg = synIDattr(synIDtrans(hlID('Normal')), 'bg')
    execute "highlight! LineNr ctermfg=" . l:margin_fg . " ctermbg=" . l:margin_bg
    highlight! link CursorLineNr LineNr
    highlight! link SignColumn LineNr
    execute "highlight! FoldColumn ctermfg=" . l:margin_bg . " ctermbg=" . l:margin_bg

    " Make linter errors and messages clearly visible.
    execute "highlight! NeomakeMessageSign ctermfg=4 ctermbg=" . l:margin_bg
    execute "highlight! NeomakeInfoSign    ctermfg=2 ctermbg=" . l:margin_bg
    execute "highlight! NeomakeWarningSign ctermfg=3 ctermbg=" . l:margin_bg
    execute "highlight! NeomakeErrorSign   ctermfg=1 ctermbg=" . l:margin_bg

    " Make folds (and folded line numbers) less conspicuous.
    highlight! link Folded LineNr

    " Make split dividers less conspicuous.
    execute "highlight! VertSplit ctermfg=" . l:margin_fg . " ctermbg=" . l:margin_fg

    " Make TODO, FIXME, etc. stand out.
    highlight Todo term=reverse cterm=reverse ctermfg=5
endfunction

" Revert to original colorscheme.
function! s:reset_colorscheme()
    execute "colorscheme " . g:colors_name

    " Fix neomake sign background to match the sign column.
    let l:margin_bg = synIDattr(synIDtrans(hlID('SignColumn')), 'bg')
    execute "highlight! NeomakeMessageSign ctermfg=4 ctermbg=" . l:margin_bg
    execute "highlight! NeomakeInfoSign    ctermfg=2 ctermbg=" . l:margin_bg
    execute "highlight! NeomakeWarningSign ctermfg=3 ctermbg=" . l:margin_bg
    execute "highlight! NeomakeErrorSign   ctermfg=1 ctermbg=" . l:margin_bg
endfunction

" Save original margin settings.
function! s:save_old_margins()
    " Save previous settings, so they can be restored when appropriate.
    let s:prev_number = &number
    let s:prev_relativenumber = &relativenumber
    let s:prev_numberwidth = &numberwidth
    let s:prev_foldcolumn = &foldcolumn
endfunction

" Determine new margin settings.
function! s:calculate_new_margins()
    if exists('g:focus_width')
        let l:focus_width = max([g:focus_width, &textwidth])
    else
        let l:focus_width = &textwidth
    endif
    let l:desired_margin = (&columns - l:focus_width)/2
    let l:left_margin = max([4 + 2 + 0, min([10 + 2 + 12, l:desired_margin])])

    let s:number = 0
    let s:relativenumber = 1
    let s:numberwidth = max([4, min([10, l:left_margin - 2 - &foldcolumn])])
    let s:foldcolumn = max([0, min([12, l:left_margin - 4 - 2])])
endfunction

" Set margins to offset/center the text area.
function! s:set_margins()
    let &number = s:number
    let &relativenumber = s:relativenumber
    let &numberwidth = s:numberwidth
    let &foldcolumn = s:foldcolumn
    " execute "set foldcolumn=" . max([0, min([12, l:left_margin - 4 - 2])])
    " execute "set numberwidth=" . max([4, min([10, l:left_margin - 2 - &foldcolumn])])
endfunction

" Revert margins to the original settings.
function! s:reset_margins()
    let &number = s:prev_number
    let &relativenumber = s:prev_relativenumber
    let &numberwidth = s:prev_numberwidth
    let &foldcolumn = s:prev_foldcolumn
endfunction

" Turn on Focus Mode.
function! s:focus_on()
    if s:focus_state == "on" || &modifiable == 0
        return
    endif
    let s:focus_state = "on"

    " Register callbacks.
    augroup focus
        autocmd!
        autocmd ColorScheme * call <SID>patch_colorscheme()
        autocmd VimResized  * call <SID>calculate_margins() | call <SID>set_margins()
        autocmd BufNewFile,BufWinEnter * call <SID>set_margins()
    augroup END

    " Apply Focus Mode changes.
    call <SID>patch_colorscheme()
    call <SID>save_old_margins()
    call <SID>calculate_new_margins()
    call <SID>set_margins()
endfunction

" Turn off Focus Mode.
function! s:focus_off()
    if s:focus_state == "off" || &modifiable == 0
        return
    endif
    let s:focus_state = "off"

    " Register callbacks.
    augroup focus
        autocmd!
        autocmd BufNewFile,BufWinEnter * call <SID>reset_margins()
    augroup END

    " Undo Focus Mode changes.
    call <SID>reset_colorscheme()
    call <SID>reset_margins()
endfunction

" Toggle Focus Mode.
function! s:focus_toggle()
    if s:focus_state == "off"
        call <SID>focus_on()
    else
        call <SID>focus_off()
    endif
endfunction

" Key binding to toggle Focus Mode.
if exists('g:focus_toggle_key')
    execute "noremap " . g:focus_toggle_key . " :call <SID>focus_toggle()<CR>i<Esc>`^"
endif
