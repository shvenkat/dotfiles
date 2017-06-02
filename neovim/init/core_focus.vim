let g:focus_toggle_key = "<leader>f"


" ----------  Simple Focus Mode  ---------------------------------------------

" Tracks Focus Mode state transitions.
let s:focus_state = "null"


" Apply Focus Mode settings once. Adjusts the left margin and colorscheme.
function! s:focus_do()
    " ----  ADJUST THE LEFT MARGIN TO CENTER (OR OFFSET) THE TEXT AREA.  ----

    " Save previous settings if needed, so they can be restored when appropriate.
    if ! exists('s:focus_state') || s:focus_state != "on"
        let s:prev_numberwidth = &numberwidth
        let s:prev_foldcolumn = &foldcolumn
    endif
    let s:focus_state = "on"

    " Make the number column visible so it is available to adjust the left margin.
    setlocal nonumber relativenumber

    " Calculate the left margin width.
    if exists('g:focus_width')
        let l:focus_width = max([g:focus_width, &textwidth])
    else
        let l:focus_width = &textwidth
    endif
    let l:desired_margin = (&columns - l:focus_width)/2
    let l:left_margin = max([4 + 2 + 0, min([10 + 2 + 12, l:desired_margin])])

    " Set the left margin width.
    execute "setlocal foldcolumn=" . max([0, min([12, l:left_margin - 4 - 2])])
    execute "setlocal numberwidth=" . max([4, min([10, l:left_margin - 2 - &foldcolumn])])

    " ----  MODIFY THE COLORSCHEME TO FOCUS ATTENTION ON THE TEXT AREA.  ----

    " execute "colorscheme " . g:colors_name

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

    " Make TODO, FIXME, etc. stand out.
    highlight Todo term=reverse cterm=reverse ctermfg=5
endfunction


" Reverts Focus Mode. Restores the left margin and colorscheme.
function! s:focus_undo()
    " Restore the left margin settings.
    if exists('s:prev_numberwidth')
        execute "setlocal numberwidth=" . s:prev_numberwidth
        execute "setlocal foldcolumn=" . s:prev_foldcolumn
    endif
    let s:focus_state = "off"

    " Undo colorscheme modifications.
    execute "colorscheme " . g:colors_name

    " Fix neomake sign background to match the sign column.
    let l:margin_bg = synIDattr(synIDtrans(hlID('SignColumn')), 'bg')
    execute "highlight! NeomakeMessageSign ctermfg=4 ctermbg=" . l:margin_bg
    execute "highlight! NeomakeInfoSign    ctermfg=2 ctermbg=" . l:margin_bg
    execute "highlight! NeomakeWarningSign ctermfg=3 ctermbg=" . l:margin_bg
    execute "highlight! NeomakeErrorSign   ctermfg=1 ctermbg=" . l:margin_bg
endfunction


" Turns on Focus Mode. Settings will apply to all buffers and colorschemes.
function! s:focus_on()
    " Register the focus_do() callback.
    augroup focus
        autocmd!
        autocmd ColorScheme,VimResized,BufNewFile,BufWinEnter * call <SID>focus_do()
    augroup END
    " Enable focus mode by triggering an event.
    execute "colorscheme " . g:colors_name
endfunction


" Turns off Focus Mode. Previous settings will apply to all buffers and colorschemes.
function! s:focus_off()
    " Un-register the focus_do() callback.
    autocmd! focus
    " Disable focus mode.
    call <SID>focus_undo()
endfunction


" Toggles Focus Mode.
function! s:focus_toggle()
    if ! exists('s:focus_state') || s:focus_state != "on"
        call <SID>focus_on()
    else
        call <SID>focus_off()
    endif
endfunction


" Key binding to toggle Focus Mode.
if exists('g:focus_toggle_key')
    execute "noremap " . g:focus_toggle_key . " :call <SID>focus_toggle()<CR>i<Esc>`^"
endif
