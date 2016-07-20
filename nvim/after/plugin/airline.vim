function! TypeEncodingFormat()
  let enc = &fenc == 'utf-8' ? '' : &fenc
  let fmt = &ff == 'unix' ? '' : &ff
  let enc_fmt = enc.fmt == '' ? '' : enc.'/'.fmt.' '
  return printf('%s%s', enc_fmt, &ft)
endfunction

call airline#parts#define_function('paste', 'airline#parts#paste')
let g:airline_section_a = airline#section#create_left(['paste'])

call airline#parts#define_function('tagbar', 'airline#extensions#tagbar#currenttag')
let g:airline_section_x = airline#section#create(['tagbar'])

call airline#parts#define_function('ffenc', 'TypeEncodingFormat')
let g:airline_section_y = airline#section#create_right(['ffenc'])

let g:airline_section_z = airline#section#create(['windowswap', '%l:%2v'])
