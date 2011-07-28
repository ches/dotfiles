" Like the pep8 one, nabbed from here:
"
"   http://code.google.com/p/vim-soko/source/browse/trunk/vimfiles/compiler/pep8.vim
"
" Same caveat comments apply as with pep8!

" pyflakes: passive checker of Python programs
" http://pypi.python.org/pypi/pyflakes

if exists("current_compiler")
  finish
endif
let current_compiler = "pyflakes"

CompilerSet makeprg=pyflakes
      \\ $*
      \\ %

CompilerSet errorformat=
      \%f:%l:\ %m

