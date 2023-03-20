let SessionLoad = 1
let s:so_save = &g:so | let s:siso_save = &g:siso | setg so=0 siso=0 | setl so=-1 siso=-1
let v:this_session=expand("<sfile>:p")
silent only
silent tabonly
cd ~/Documents/bachelor_thesis/language
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
let s:shortmess_save = &shortmess
if &shortmess =~ 'A'
  set shortmess=aoOA
else
  set shortmess=aoO
endif
badd +1 ~/Documents/bachelor_thesis/language
badd +298 src/TypeChecker/TypeChecker.hs
badd +7 test_program
badd +46 src/TypeChecker/TypeCheckerIr.hs
badd +6 Grammar.cf
badd +1 src/Grammar/Abs.hs
argglobal
%argdel
$argadd ~/Documents/bachelor_thesis/language
set stal=2
tabnew +setlocal\ bufhidden=wipe
tabnew +setlocal\ bufhidden=wipe
tabnew +setlocal\ bufhidden=wipe
tabrewind
edit src/TypeChecker/TypeChecker.hs
let s:save_splitbelow = &splitbelow
let s:save_splitright = &splitright
set splitbelow splitright
wincmd _ | wincmd |
vsplit
1wincmd h
wincmd w
let &splitbelow = s:save_splitbelow
let &splitright = s:save_splitright
wincmd t
let s:save_winminheight = &winminheight
let s:save_winminwidth = &winminwidth
set winminheight=0
set winheight=1
set winminwidth=0
set winwidth=1
exe 'vert 1resize ' . ((&columns * 99 + 86) / 173)
exe 'vert 2resize ' . ((&columns * 73 + 86) / 173)
argglobal
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let &fdl = &fdl
let s:l = 298 - ((18 * winheight(0) + 21) / 42)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 298
normal! 029|
lcd ~/Documents/bachelor_thesis/language
wincmd w
argglobal
if bufexists(fnamemodify("~/Documents/bachelor_thesis/language/Grammar.cf", ":p")) | buffer ~/Documents/bachelor_thesis/language/Grammar.cf | else | edit ~/Documents/bachelor_thesis/language/Grammar.cf | endif
if &buftype ==# 'terminal'
  silent file ~/Documents/bachelor_thesis/language/Grammar.cf
endif
balt ~/Documents/bachelor_thesis/language/src/TypeChecker/TypeChecker.hs
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let &fdl = &fdl
let s:l = 7 - ((6 * winheight(0) + 21) / 42)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 7
normal! 0
lcd ~/Documents/bachelor_thesis/language
wincmd w
exe 'vert 1resize ' . ((&columns * 99 + 86) / 173)
exe 'vert 2resize ' . ((&columns * 73 + 86) / 173)
tabnext
edit ~/Documents/bachelor_thesis/language/src/TypeChecker/TypeCheckerIr.hs
let s:save_splitbelow = &splitbelow
let s:save_splitright = &splitright
set splitbelow splitright
wincmd _ | wincmd |
vsplit
1wincmd h
wincmd w
let &splitbelow = s:save_splitbelow
let &splitright = s:save_splitright
wincmd t
let s:save_winminheight = &winminheight
let s:save_winminwidth = &winminwidth
set winminheight=0
set winheight=1
set winminwidth=0
set winwidth=1
exe 'vert 1resize ' . ((&columns * 86 + 86) / 173)
exe 'vert 2resize ' . ((&columns * 86 + 86) / 173)
argglobal
balt ~/Documents/bachelor_thesis/language/test_program
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let &fdl = &fdl
let s:l = 1 - ((0 * winheight(0) + 21) / 42)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 1
normal! 0
lcd ~/Documents/bachelor_thesis/language
wincmd w
argglobal
if bufexists(fnamemodify("~/Documents/bachelor_thesis/language/src/Grammar/Abs.hs", ":p")) | buffer ~/Documents/bachelor_thesis/language/src/Grammar/Abs.hs | else | edit ~/Documents/bachelor_thesis/language/src/Grammar/Abs.hs | endif
if &buftype ==# 'terminal'
  silent file ~/Documents/bachelor_thesis/language/src/Grammar/Abs.hs
endif
balt ~/Documents/bachelor_thesis/language/src/TypeChecker/TypeCheckerIr.hs
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let &fdl = &fdl
let s:l = 1 - ((0 * winheight(0) + 21) / 42)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 1
normal! 0
lcd ~/Documents/bachelor_thesis/language
wincmd w
exe 'vert 1resize ' . ((&columns * 86 + 86) / 173)
exe 'vert 2resize ' . ((&columns * 86 + 86) / 173)
tabnext
edit ~/Documents/bachelor_thesis/language/Grammar.cf
argglobal
balt ~/Documents/bachelor_thesis/language/src/Grammar/Abs.hs
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let &fdl = &fdl
let s:l = 40 - ((12 * winheight(0) + 21) / 42)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 40
normal! 0
lcd ~/Documents/bachelor_thesis/language
tabnext
edit ~/Documents/bachelor_thesis/language/test_program
argglobal
balt ~/Documents/bachelor_thesis/language/src/TypeChecker/TypeChecker.hs
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let &fdl = &fdl
let s:l = 7 - ((6 * winheight(0) + 21) / 42)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 7
normal! 010|
lcd ~/Documents/bachelor_thesis/language
tabnext 1
set stal=1
if exists('s:wipebuf') && len(win_findbuf(s:wipebuf)) == 0 && getbufvar(s:wipebuf, '&buftype') isnot# 'terminal'
  silent exe 'bwipe ' . s:wipebuf
endif
unlet! s:wipebuf
set winheight=1 winwidth=20
let &shortmess = s:shortmess_save
let s:sx = expand("<sfile>:p:r")."x.vim"
if filereadable(s:sx)
  exe "source " . fnameescape(s:sx)
endif
let &g:so = s:so_save | let &g:siso = s:siso_save
set hlsearch
nohlsearch
doautoall SessionLoadPost
unlet SessionLoad
" vim: set ft=vim :
