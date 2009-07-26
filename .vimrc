" An example for a vimrc file.
"
" Maintainer:	Bram Moolenaar <Bram@vim.org>
" Last change:	2002 Sep 19
"
" To use it, copy it to
"     for Unix and OS/2:  ~/.vimrc
"	      for Amiga:  s:.vimrc
"  for MS-DOS and Win32:  $VIM\_vimrc
"	    for OpenVMS:  sys$login:.vimrc

" Customized from the example cited above, mostly with help from:
" http://pida.co.uk/trac/wiki/ConfiguringVimForPython

" When started as "evim", evim.vim will already have done these settings.
if v:progname =~? "evim"
  finish
endif

" General options {{{
" Miscellaneous and Display {{{

" Use Vim settings, rather then Vi settings (much better!).
" This must be first, because it changes other options as a side effect.
set nocompatible

" allow backspacing over everything in insert mode
set backspace=indent,eol,start

if has("vms")
  set nobackup		" do not keep a backup file, use versions instead
else
  set backup		" keep a backup file
endif
set history=50		" keep 50 lines of command line history
set ruler		    " show the cursor position all the time
set showcmd		    " display incomplete commands
set incsearch		" do incremental searching
set ignorecase      " Do case insensitive matching
set smartcase       " Do smart case matching

" line numbers
set nu

" automatically flush to disk when using :make, etc.
set autowrite

" threshold for reporting number of lines changed
set report=0

" For modern standards in :TOhtml output
let html_use_css=1
let use_html=1

" For Win32 GUI: remove 't' flag from 'guioptions': no tearoff menu entries
" let &guioptions = substitute(&guioptions, "t", "", "g")

" Don't use Ex mode, use Q for formatting
map Q gq

" This is an alternative that also works in block mode, but the deleted
" text is lost and it only works for putting the current register.
"vnoremap p "_dp

" Switch syntax highlighting on, when the terminal has colors
" Also switch on highlighting the last used search pattern.
if &t_Co > 2 || has("gui_running")
  syntax on
  set hlsearch
  set cursorline
  colorscheme oceanblack
endif

" Only do this part when compiled with support for autocommands.
if has("autocmd")

  " Enable file type detection.
  " Use the default filetype settings, so that mail gets 'tw' set to 72,
  " 'cindent' is on in C files, etc.
  " Also load indent files, to automatically do language-dependent indenting.
  filetype plugin indent on

  " Put these in an autocmd group, so that we can delete them easily.
  augroup vimrcEx
  au!

  " For all text files set 'textwidth' to 78 characters.
  autocmd FileType text setlocal textwidth=78

  augroup END

else

  set autoindent		" always set autoindenting on

endif " has("autocmd")
"}}}

" Indentation {{{

" no-longer skinny tabs!
set tabstop=4

" set to the same as tabstop (see #4 in :help tabstop)
set shiftwidth=4

" if it looks like a tab, we can delete it like a tab
set softtabstop=4

" no tabs! spaces only..
set expandtab

" do not break lines when line lenght increases
set textwidth=0

" < and > will hit indentation levels instead of always -4/+4
set shiftround

" braces affect autoindentation
set smartindent

" Show matching brackets.
set showmatch
set matchtime=2

" figure out indent when ; is pressed
set cinkeys+=;

" align break with case in a switch
"set cinoptions+=b1
"}}}

" Folding {{{

" fold only when I ask for it damnit!
""set foldmethod=marker

" close a fold when I leave it
""set foldclose=all
"}}}

" Colors {{{

highlight LineNr  term=underline    ctermfg=grey    guifg=grey
highlight CursorLine    guibg=Grey10
" No hideous pink default autocomplete menu
highlight PMenu gui=bold guibg=#CECECE guifg=#444444
"}}}

" Autocommands {{{
if has("autocmd")

  " When editing a file, always jump to the last known cursor position. {{{
  " Don't do it when the position is invalid or when inside an event handler
  " (happens when dropping a file on gvim).
  autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal g`\"" |
    \ endif
  "}}}
  
  " Skeletons {{{
  autocmd BufNewFile *.py silent 0read ~/.vim/skeleton/skeleton.py   | normal G
  "autocmd BufNewFile *.pl silent 0read ~/.vim/skeleton/perl.pl     | normal G
  "autocmd BufNewFile *.t  silent 0read ~/.vim/skeleton/perl-test.t | normal G
  "autocmd BufNewFile *.c  silent 0read ~/.vim/skeleton/c.c         | normal 4j$
  "autocmd BufNewFile *.hs silent 0read ~/.vim/skeleton/haskell.hs  | normal Gk$
  "}}}

  " Auto +x {{{
  au BufWritePost *.sh !chmod +x %
  au BufWritePost *.pl !chmod +x %
  "}}}

  " Automatically distribute my vimrc to the servers I use {{{
  "autocmd BufWritePost ~/.vimrc !scp ~/.vimrc valleyofwind.dyndns.org:.
  "autocmd BufWritePost ~/.vimrc !scp ~/.vimrc eidolos@crawl.akrasiac.org:.eidovimrc
  "autocmd BufWritePost ~/.vim/skeletons/* !scp % valleyofwind.dyndns.org:.vim/skeletons/
  "autocmd BufWritePost ~/.vim/skeletons/* !scp % eidolos@crawl.akrasiac.org:.vim/skeletons/
  "}}}

endif " has("autocmd")
"}}}

" Remappings {{{

" Code completion shortcut
imap <f3> <C-x><C-o><C-p>

"}}}
"}}}

" vim:foldmethod=marker commentstring="%s

