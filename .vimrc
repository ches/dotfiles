" Initially based on the Vim distribution's example vimrc, with
" additional inspiration from all over the web.

" When started as "evim", evim.vim will already have done these settings.
if v:progname =~? "evim"
  finish
endif

" General options {{{1
" Miscellaneous and Display {{{2

" Use Vim settings, rather then Vi settings (much better!).
" This must be first, because it changes other options as a side effect.
set nocompatible

" allow backspacing over everything in insert mode
set backspace=indent,eol,start

if has("vms")
  set nobackup                  " do not keep a backup file, use versions instead
else
  set backup                    " keep a backup file
  set backupdir=~/.backup/vim   " leave all the droppings in one place
endif

set history=500     " keep more command line history
set ruler           " show the cursor position all the time
set showcmd         " display commands as they're being entered
set incsearch       " do incremental searching
set ignorecase      " Do case insensitive matching
set smartcase       " But if search contains capitals, be sensitive
set scrolloff=3     " Keep some context visible when scrolling
set wildmenu        " Modern completion menu
set number          " line numbers
set numberwidth=5   " a little bit of buffer is prettier

" wildmenu does shell-style completion AND tab-through
set wildmode=list:longest,full

" Ignore some extensions when tab-completing
set wildignore=*.swp,*.bak,*.pyc,*.o,*.class

" automatically flush to disk when using :make, changing buffers, etc.
" Alternatively, set hidden to allow moving around and leaving dirty files be
"set autowrite
set hidden

" use existing window if I try to open an already-open buffer
set switchbuf=useopen

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

" Pathogen makes Vim plugin management saner:
" http://www.vim.org/scripts/script.php?script_id=2332
" Heed the install notes for system vim defaults on Linux.
call pathogen#runtime_append_all_bundles()

" Switch syntax highlighting on, when the terminal has colors
" Also switch on highlighting the last used search pattern.
if &t_Co > 2 || has("gui_running")
  syntax on
  set hlsearch
  set cursorline
  colorscheme customtwilight  " last used: oceanblack
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

  set autoindent        " always set autoindenting on

endif " has("autocmd")

if has("viminfo")

  " Allow some global variables to persist between sessions
  " Plugins sometimes use this to retain useful things
  " % saves and restores buffer list when started with no args
  set viminfo^=!,%

endif

" Indentation {{{2

" no-longer skinny tabs!
set tabstop=4

" set to the same as tabstop (see #4 in :help tabstop)
set shiftwidth=4

" if it looks like a tab, we can delete it like a tab
set softtabstop=4

" no tabs! spaces only..
set expandtab

" do not break lines when line length increases
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

" Use attractive characters to show tabs & trailing spaces
set listchars=tab:»·,trail:·,eol:¬,nbsp:␣

" Folding {{{2

" fold only when I ask for it damnit!
""set foldmethod=marker

" close a fold when I leave it
""set foldclose=all

" Colors {{{2

" These might be desired depending on colorscheme
"highlight LineNr  term=underline    ctermfg=grey    guifg=grey
"highlight CursorLine    guibg=Grey10
" No hideous pink default autocomplete menu
"highlight PMenu gui=bold guibg=#CECECE guifg=#444444

" Autocommands {{{2
if has("autocmd")

  " When editing a file, always jump to the last known cursor position. {{{
  " Don't do it when the position is invalid or when inside an event handler
  " (happens when dropping a file on gvim), or for commit messages.
  autocmd BufReadPost * call SetCursorPosition()
  function! SetCursorPosition()
    if &filetype !~ 'commit\c'
      if line("'\"") > 0 && line("'\"") <= line("$")
        exe "normal g`\""
        normal! zz
      endif
    end
  endfunction
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
" Remappings {{{1

" I'm drinkin' the comma-as-leader kool aid
let mapleader = ","

" Code completion shortcut
imap <f3> <C-x><C-o><C-p>

" Easy paste mode toggling
set pastetoggle=<F6>

" Toggle search hilighting
map <silent> <F11> :set invhlsearch<CR>
imap <silent> <F11> <C-o>:set invhlsearch<CR>
vmap <silent> <F11> :<C-u>set invhlsearch<CR>gv

" It's a fast-moving world these days -- does your scrolling keep up?
noremap <C-y> 2<C-y>
noremap <C-e> 2<C-e>

" Yank from cursor to end of line, to be consistent with C and D.
nnoremap Y y$

" Why so much hand lifting pain for command editing?
" Allow Emacs/readline-like keys.
cnoremap <C-A>      <Home>
cnoremap <C-B>      <Left>
cnoremap <C-E>      <End>
cnoremap <C-F>      <Right>
cnoremap <C-N>      <Down>
cnoremap <C-P>      <Up>
if has('mac')
  cnoremap <M-b>      <S-Left>
  cnoremap <M-f>      <S-Right>
  cnoremap <M-BS>     <C-W>
else
  cnoremap <ESC>b     <S-Left>
  cnoremap <ESC><C-B> <S-Left>
  cnoremap <ESC>f     <S-Right>
  cnoremap <ESC><C-F> <S-Right>
  cnoremap <ESC><C-H> <C-W>
endif

" Easy window split navigation {{{
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l
map <C-h> <C-w>h

" Toggle a window's height stickiness, so C-w = doesn't equalize it
nmap <leader>` :set invwinfixheight winfixheight?<CR>
"}}}

" Lotsa TextMate-inspired Mappings
source ~/.vim/include/textmate-mappings.vim

" Language- and plugin-specific Preferences {{{1
if has("autocmd")

  " FileType Stuff {{{

  " Use leader+space to write and execute
  autocmd FileType vim map <buffer> <leader><space> :w!<cr>:source %<cr>
  autocmd FileType ruby map <buffer> <leader><space> :w!<cr>:!ruby %<cr>
  autocmd FileType python map <buffer> <leader><space> :w!<cr>:!python %<cr>

  " autocmd FileType python setlocal cinwords=if,elif,else,for,while,try,except,finally,def,class,with
  autocmd FileType javascript,ruby,vim,yaml setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2

  let javascript_enable_domhtmlcss=1
  let xml_use_xhtml = 1                     " default xml to self-closing tags

  autocmd FileType html,xhtml,xml,htmldjango,htmljinja,eruby,mako,cucumber setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2
  autocmd BufNewFile,BufRead jquery.*.js set ft=javascript syntax=jquery
  autocmd BufNewFile,BufRead *.mako setlocal ft=mako

  autocmd FileType mkd set ai formatoptions=tcroqn2 comments=n:>

  "}}}

  " NERDTree
  let NERDTreeWinPos          = 'right'
  let NERDTreeShowBookmarks   = 1
  let NERDTreeQuitOnOpen      = 1      " hide after opening a file
  let NERDTreeMapActivateNode = '<CR>' " step in with Enter in addition to 'o'
  let NERDTreeIgnore          = ['\.git','\.hg','\.svn','\.DS_Store']
  let NERDTreeHijackNetrw     = 0      " I like netrw when I `:e somedir`

  "NERDCommenter
  let NERDSpaceDelims  = 1             " use a space after comment chars
  let NERDDefaultAlign = 'left'

  " Change default TaskList invocation, conflicts with Command-T plugin
  map <leader>T <Plug>TaskList
  " TaskList on bottom (open with <leader>T)
  let g:tlWindowPosition      = 1
  " Custom TaskList tokens
  let g:tlTokenList = ['FIXME', 'TODO', 'CHANGED', 'PONY']

  " Open the YankRing window
  nnoremap <silent> <M-v> :YRShow<CR>

  " Lusty Juggler buffer switcher
  let g:LustyJugglerShowKeys = 'a'
  let g:LustyJugglerSuppressRubyWarning = 1
  nmap <silent> <leader>b :LustyJuggler<CR>

  " Gist
  let g:gist_put_url_to_clipboard_after_post  = 1
  let g:gist_show_privates                    = 1
  " detect filetype if vim failed autodetection
  let g:gist_detect_filetype                  = 1
  if has('mac')
    let g:gist_clip_command                   = 'pbcopy'
  endif

endif " has("autocmd")

" Plugin Mappings {{{2

" Ack Search
map <Leader>a :Ack<space>

" NERD tree - double-leader
map <Leader><Leader> :NERDTreeToggle<cr>

" Ready for tab-completion of named Tabular patterns
" Choosing 'gq' since it's similar function to the format command
map <Leader>gq :Tabularize<space>

if has('python')
  " UltiSnips
  let g:UltiSnipsExpandTrigger       = "<tab>"
  let g:UltiSnipsJumpForwardTrigger  = "<tab>"
  let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"

  " Gundo
  nnoremap <F5> :GundoToggle<CR>
  let g:gundo_preview_bottom = 1     " force wide window across bottom

else

  let g:gundo_disable = 1

endif

" VCSCommand {{{3

" The defaults (prefix of <leader>c) conflict with NERDCommenter, and
" I don't really like them anyway...
" FIXME: the bang commands don't work
map <Leader>va <Plug>VCSAdd
" 'Blame' is the most natural mnemonic for me
map <Leader>vb <Plug>VCSAnnotate
map <Leader>vB <Plug>VCSAnnotate!
map <Leader>vc <Plug>VCSCommit
map <Leader>vD <Plug>VCSDelete
map <Leader>vd <Plug>VCSDiff
map <Leader>vv <Plug>VCSVimDiff
" Close VCS scratch buffer and return - mnemonic: eXit :-)
map <Leader>vx <Plug>VCSGotoOriginal!
map <Leader>vl <Plug>VCSLog
" I think I'm far too likely to try 'r' for 'remove' all the time...
map <Leader>vR <Plug>VCSReview
map <Leader>vs <Plug>VCSStatus

" Specky - RSpec plugin {{{3

let g:speckyBannerKey = "<C-S>b"
let g:speckyQuoteSwitcherKey = "<C-S>'"
let g:speckyRunRdocKey = "<C-S>r"
let g:speckySpecSwitcherKey = "<C-S>x"
let g:speckyRunSpecKey = "<C-S>s"
"let g:speckyRunSpecCmd = "spec -fs"
let g:speckyRunRdocCmd = "qri -f plain"
let g:speckyWindowType = 1      " Horizontal split

" Custom Functions {{{1

if has('mac')
  let g:browser = 'open '

  " Command that Rails.vim uses for various browser-opening functions
  command -bar -nargs=1 OpenURL :!open <args>
endif

" Open the Rails ApiDock page for the word under cursor
function! OpenRailsDoc(keyword)
  let url = 'http://apidock.com/rails/'.a:keyword
  exec '!'.g:browser.' '.url
endfunction

" Open the Ruby ApiDock page for the word under cursor
function! OpenRubyDoc(keyword)
  let url = 'http://apidock.com/ruby/'.a:keyword
  exec '!'.g:browser.' '.url
endfunction

" Easily lookup documentation on apidock
noremap <leader>rb :call OpenRubyDoc(expand('<cword>'))<CR>
noremap <leader>rr :call OpenRailsDoc(expand('<cword>'))<CR>

" vim:foldmethod=marker commentstring="%s

