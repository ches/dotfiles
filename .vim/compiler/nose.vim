" Vim compiler file
" Compiler: Unit testing for Python using nose
" Maintainer: Olivier Le Thanh Duong <olivier@lethanh.be>
" Last Change: 2010 Sep 1

" Based on pyunit.vim distributed with vim
" Compiler: Unit testing tool for Python
" Maintainer: Max Ischenko <mfi@ukr.net>
" Last Change: 2004 Mar 27

if exists("current_compiler")
  finish
endif
let current_compiler = "nose"

if exists(":CompilerSet") != 2    " older Vim always used :setlocal
  command -nargs=* CompilerSet setlocal <args>
endif

" Modified from pyunit, remove other lines from quickfix window
CompilerSet efm=%-C\ %.%#,%A\ \ File\ \"%f\"\\,\ line\ %l%.%#,%Z%[%^\ ]%\\@=%m,%-G%.%#

"" Set nose as default compiler
" CompilerSet makeprg=nosetests
" Quite ugly but this make it ignore vim-makegreen passing argument for now
CompilerSet makeprg=echo\ $*\ >/dev/null;\ nosetests

