" Nabbed from:
" http://code.google.com/p/vim-soko/source/browse/trunk/vimfiles/compiler/pep8.vim
"
" Will play around with this vs. pep8 plugin -- I like to have the simple
" :compiler/:make usage here, just need to make it silent and add automatic
" opening of quickfix list, similar to this:
"
"    http://www.jfroche.be/blogging/archive/2007/04/28/write-nicer-python-code
"
" And see this same guy's DoLint function here:
"
"    http://code.google.com/p/vim-soko/source/browse/trunk/vimfiles/vimrc.vim
"
" Alternatively maybe add pep8 to syntastic.

" pep8 - Python style guide checker
" http://pypi.python.org/pypi/pep8

if exists("current_compiler")
  finish
endif
let current_compiler = "pep8"

CompilerSet makeprg=pep8
      \\ $*
      \\ %

CompilerSet errorformat=
      \%f:%l:%c:\ %m

