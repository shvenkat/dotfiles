

" ----------  Folding  -------------------------------------------------------

set foldenable
set foldlevelstart=99    " Start with all folds open.

" For a folded block, display the first line with ellipses.
function! FirstLineWithEllipses()
  " Returns the text of the first line of a folded block, with ellipses, and
  " truncated/padded to fit.
  " Based on https://gist.github.com/sjl/3360978.
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
set foldtext=FirstLineWithEllipses()
