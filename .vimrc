"
"                              ,---.
"     ,--.   ,--.,--.          |   |
"      \  `.'  / `--',--,--,--.|  .'      Ches Martin
"       \     /  ,--.|        ||  |       http://chesmart.in
"        \   /   |  ||  |  |  |`--'
"         `-'    `--'`--`--`--'.--.
"                              '--'
"

" In the extreme unlikelihood that anyone runs evim on my system,
" it will ignore all of this anyway.
if v:progname =~? "evim" | finish | endif

" Runtime + Plugin System Bootstrap {{{1
" --------------------------------------

" Vim sets $MYVIMRC, make it easy to get to stuff in ~/.vim too.
if has('nvim')
  let $MYVIMRUNTIME = expand('<sfile>:p:h')  " init.vim is child not sibling
else
  let $MYVIMRUNTIME = expand('<sfile>:p:h') . '/.vim'
endif

if has('vim_starting')
  " Vim, not Vi. This should be set first.
  set nocompatible
  set runtimepath+=~/.vim/bundle/neobundle.vim
endif

" Register and load plugins.
runtime include/bundles.vim

" Built-ins. Some things moved in recent Vim versions with package support.
if has('packages') && !has('nvim') " Neovim hasn't moved them yet
  packadd! editexisting            " Bring forward existing session w/ open file
  packadd! matchit                 " More powerful % for if/fi, HTML tags, etc.
else
  runtime macros/editexisting.vim
  runtime macros/matchit.vim
endif

runtime ftplugin/man.vim           " Sweet :Man command opens pages in split

" Allow plugins to work their magic.
filetype plugin indent on

" Check and prompt for any plugins pending installation.
NeoBundleCheck

" Options {{{1
" ------------
" Miscellaneous and Display {{{2

" allow backspacing over everything in insert mode
set backspace=indent,eol,start

" One place for backup and swap files, but we'll try a project-local .backup
" directory first if it exists. TODO: appropriate paths for Windows
let s:my_backups_rootdir = $HOME . '/.autosave/vim'
let s:my_swapfiles_dir = s:my_backups_rootdir . '/swap'

if !isdirectory(s:my_swapfiles_dir)
  call mkdir(s:my_swapfiles_dir, 'p')
endif

" // make swap files unique based on path
set directory=./.backup//,~/.autosave/vim/swap//

set backup          " keep a backup file
set backupdir=./.backup,~/.autosave/vim

set history=500     " keep more command line history
set ruler           " show the cursor position all the time
set showcmd         " display commands as they're being entered
set nomodeline      " use securemodelines
set incsearch       " do incremental searching
set ignorecase      " Do case insensitive matching
set smartcase       " But if search contains capitals, be sensitive
set gdefault        " Line-global substitution by default, usually what you want
set scrolloff=3     " Keep some context visible when scrolling
set sidescrolloff=4
set wildmenu        " Modern completion menu
set nowrap          " Default to no visual line
let &showbreak='↪  '
set number          " line numbers
set numberwidth=5   " a little bit of buffer is prettier
set lazyredraw      " Smoother display on complex ops (plugins)

" wildmenu does shell-style completion AND tab-through
set wildmode=list:longest,full

" Ignore some extensions when tab-completing
set wildignore=*.swp,*.bak,*.pyc,*.o,*.class

" Only insert up to longest common autocomplete match
set completeopt+=longest

" Basically the default statusline when ruler is enabled, with fugitive
set statusline=%<%f\ %h%m%r%{fugitive#statusline()}%=%-14.(%l,%c%V%)\ %P

" Ensure Airline is always on, instead of only appearing when there's a split.
set laststatus=2
set noshowmode
set ttimeout
set ttimeoutlen=30

set hidden                  " Allow changing buffers when a file is unwritten
set autoread                " If file changed outside vim but not inside, just read it
set switchbuf=useopen       " Use existing window if I try to open an already-open buffer
set splitbelow splitright   " New h/v split windows show up on bottom/right

set mouse=a    " Try to use mouse in console, for scrolling, etc.
set report=0   " Threshold for reporting number of lines changed

" Silence CSApprox's gripes if running a vim without gui support. nvim is fine
if !has('gui') && !has('nvim')
  let g:CSApprox_loaded = 1
endif

if has('nvim')
  " neovim can automatically switch to skinny cursor in insert mode
  let $NVIM_TUI_ENABLE_CURSOR_SHAPE = 1
endif

" TODO: What is the conditional to check for terminal support? Also requires tmux 2.2
if has('termguicolors') && !has('gui_running')
  set termguicolors

  " Sigh. Needed for tmux where an xterm profile should not be used. Not
  " needed by Neovim because it left all the termcap stuff behind. See
  " :help xterm-true-color. Fuck terminals.
  if $TERM !~ 'xterm' && !has('nvim')
    set t_8f=[38;2;%lu;%lu;%lum
    set t_8b=[48;2;%lu;%lu;%lum
  endif
endif

" Switch syntax highlighting on, when the terminal has colors
" Also switch on highlighting the last used search pattern.
if &t_Co > 2 || has("gui_running")
  syntax on
  set hlsearch
  set cursorline

  if &diff
    colorscheme xoria256          " The best diff highlighting I've found
  else
    set background=dark
    colorscheme base16-default-dark
  endif
endif

" Allow global variables, marks, etc. to persist between sessions.
" % saves and restores buffer list when started with no args
if has('viminfo')
  set viminfo^=!,%
elseif has('shada')  " Neovim
  set shada^=!,%
endif

" Indentation {{{2

" Defaults. Overridden appropriately for conventions of many filetypes.

set autoindent      " use previous line's indentation
set smarttab        " use shiftwidth for indent when at beginning of line
set tabstop=4       " true Tabs display as 8 columns in most tools, but that
                    " just looks too wide
set shiftwidth=4    " set to the same as tabstop (see #4 in :help tabstop)
set softtabstop=4   " if it looks like a tab, we can delete it like a tab
set expandtab       " no tabs! spaces only
set shiftround      " < and > will hit indentation levels instead of always -4/+4
set textwidth=0     " do not break lines when line length increases
set cinkeys+=;      " figure out C indent when ; is pressed

set showmatch       " Show matching brackets.
set matchtime=2

" Use attractive characters to show tabs & trailing spaces
set listchars=tab:»·,trail:·,eol:¬,nbsp:␣

if has('patch-7.3.541')
  set formatoptions+=j   " Remove comment leader when joining lines
endif

" Folding {{{2

set foldlevelstart=99   " default to all folds open when opening a buffer
set foldnestmax=4       " don't be absurd about how deeply to nest syntax folding
set foldopen-=block     " drives me nuts that moving with ] opens folds

" Search {{{2

set grepprg=grep\ -rnH\ --exclude='.*.swp'\ --exclude='*~'\ --exclude=tags

" Enhance :grep
if executable('ag')
  set grepprg=ag\ --vimgrep\ $*
  set grepformat=%f:%l:%c:%m
elseif executable('ack')
  set grepprg=ack\ --column\ --noheading\ $*
  set grepformat=%f:%l:%c:%m
endif

" Autocommands {{{1
" -----------------

if has("autocmd")

  augroup BufActions " {{{2
    autocmd!

    " When editing a file, always jump to the last known cursor position. {{{
    " Don't do it when the position is invalid or when inside an event handler
    " (happens when dropping a file on gvim), or for commit messages.
    autocmd BufReadPost * call SetCursorPosition()
    function! SetCursorPosition()
      if &filetype !~ 'commit\c'
        if line("'\"") > 0 && line("'\"") <= line("$")
          exe 'normal g`"'
          normal! zz
        endif
      end
    endfunction "}}}

    " Easy helptags regeneration when editing my personal Vim notes
    autocmd BufRead ~/.vim/doc/my-notes.txt
          \ setlocal modifiable iskeyword=!-~,^*,^\|,^\",192-255 |
          \ map <buffer> <Leader>. :w!<CR>:helptags ~/.vim/doc<CR>

    " Almost never want to remain in paste mode after insert
    autocmd InsertLeave * if &paste | set nopaste paste? | endif

    " Try to detect `fc` where `:q!` will surprisingly result in shell execution
    autocmd BufRead */bash-fc-* echohl WarningMsg |
          \ echo "Use :cquit to abandon fc changes without executing!!" |
          \ echohl None

    " Skeletons {{{
    autocmd BufNewFile build.sbt silent 0read ~/.vim/skeleton/build.sbt| normal ggf"
    autocmd BufNewFile Cargo.toml silent 0read ~/.vim/skeleton/Cargo.toml | /^name
    autocmd BufNewFile Makefile silent 0read ~/.vim/skeleton/Makefile  | /^targets
    autocmd BufNewFile .lvimrc silent 0read ~/.vim/skeleton/lvimrc.vim | normal }j
    autocmd BufNewFile *.ino silent 0read ~/.vim/skeleton/skeleton.ino | normal 2G
    autocmd BufNewFile .projections.json
          \ silent 0read ~/.vim/skeleton/projections.json | normal 2G
    "}}}

    " Auto file perms {{{
    autocmd BufNewFile */.netrc,*/.fetchmailrc,*/.my.cnf let b:chmod_new="go-rwx"
    autocmd BufNewFile  * let b:chmod_exe=1
    autocmd BufWritePre * if exists("b:chmod_exe") |
          \ unlet b:chmod_exe |
          \ if getline(1) =~ '^#!' | let b:chmod_new="+x" | endif |
          \ endif
    autocmd BufWritePost,FileWritePost * if exists("b:chmod_new")|
          \ silent! execute "!chmod ".b:chmod_new." <afile>"|
          \ unlet b:chmod_new|
          \ endif
    "}}}
    " Automatically make scripts their own :Make/:Start runner for Dispatch
	autocmd BufReadPost *
          \ if getline(1) =~# '^#!' |
          \   let b:dispatch = getline(1)[2:-1] . ' %' | let b:start = b:dispatch |
          \ endif
  augroup END "}}}

endif " has("autocmd")

" Mappings {{{1
" -------------

" Because two hands are better than one.
let mapleader = "\<Space>"
let maplocalleader = "\\"  " Try comma or Tab? (alas, Ctrl-i interference, damn)

" In and out of command mode quickly, less pain.
noremap <CR> :

" Leader Mappings {{{2

" Lots of great ideas for fewer Shift reaches from:
" http://sheerun.net/2014/03/21/how-to-boost-your-vim-productivity/
nnoremap <Leader>w :w<CR>
nnoremap <Leader>x :xit<CR>

" Navigation with fewer chords
nnoremap <Leader><Space> <C-f>
nnoremap <Leader><BS>    <C-b>
nnoremap <Left>  {
nnoremap <Right> }
nnoremap <Up>    <C-u>
nnoremap <Down>  <C-d>

" New trial for NERDtree and Tagbar: <Leader>[ and <Leader>]; or :pop and
" :tag for these? Or <C-i>/<C-o> opening up Tab for LocalLeader...
" nnoremap <Leader>, :NERDTreeToggle<CR>
" nnoremap <Leader>. :TagbarToggle<CR>
nnoremap <Leader>[ :NERDTreeToggle<CR>
nnoremap <Leader>] :TagbarToggle<CR>

" Mnemonic: [o]pen files or [f]unctions. <Leader>b is LustyJuggler, change or drop it?
nnoremap <Leader>o      :CtrlP<CR>
nnoremap <Leader><C-o>  :CtrlPBuffer<CR>
nnoremap <Leader>f      :CtrlPBufTag<CR>
nnoremap <Leader><C-f>  :CtrlPTag<CR>
nnoremap <Leader>m      :CtrlPMRUFiles<CR>
nnoremap <Leader><CR>   :CtrlPCmdPalette<CR>

" Copy and paste to/from system clipboard with ease
vnoremap <Leader>y "+y
vnoremap <Leader>d "+d
nnoremap <Leader>p "+p
nnoremap <Leader>P "+P
vnoremap <Leader>p "+p
vnoremap <Leader>P "+P

" Edit vimrc. Use <leader>. mapping (when active buffer) to source it.
nnoremap <leader>ev :split  $MYVIMRC<CR>
nnoremap <leader>eV :vsplit $MYVIMRC<CR>
nnoremap <leader>er :split  $MYVIMRUNTIME/<CR>
nnoremap <leader>eR :vsplit $MYVIMRUNTIME/<CR>
nnoremap <leader>en :split  ~/.vim/doc/my-notes.txt<CR>
nnoremap <leader>eN :vsplit ~/.vim/doc/my-notes.txt<CR>
nnoremap <leader>ef :exec 'split '  . join([$MYVIMRUNTIME, 'after', 'ftplugin', &ft . '.vim'], '/')<CR>
nnoremap <leader>eF :exec 'vsplit ' . join([$MYVIMRUNTIME, 'after', 'ftplugin', &ft . '.vim'], '/')<CR>

" Search runtime files (plugins) -- warning: slow!
nnoremap <leader>eP :CtrlPRTS<CR>

" TODO: make this a command, something like this but proper completions:
" command -bar -nargs=? -complete=help Help help my-notes-<args>
nnoremap <leader>hh :help my-notes-

" Ready for tab-completion of named Tabular patterns
" Choosing 'gq' since it's similar function to the format command
vnoremap <Leader>gq :Tabularize<space>
vnoremap <Leader>q= :Tabularize assignment<CR>
vnoremap <Leader>q<Bar> :Tabularize bars<CR>
" }}}2

" Tried this in after/ftplugin/help.vim, let's try it globally
nnoremap <BS> <C-t>

" Omni completion shortcut
imap <M-space> <C-x><C-o><C-p>

" Builds, with dispatch.vim
"
" See tpope's wicked Run() function -- I'd like to cook up something
" similar ror ruby, pyflakes/pep8, etc.
"
" TODO: Maybe move to match new Mac media key layout of F7/8/9, but left hand
" is nicer with leader mappings... Shift versions won't work in terminal,
" surprising no one.
nnoremap <F2>         :cprev<CR>
nnoremap <F3>         :wa<Bar>Dispatch<CR>
nnoremap <F4>         :cnext<CR>
nnoremap <Leader><F2> :lprev<CR>
nnoremap <Leader><F3> :wa<Bar>Make<CR>
nnoremap <Leader><F4> :lnext<CR>

nnoremap <LocalLeader><F2> :Copen<CR>
nnoremap <LocalLeader><F3> :Start<CR>

" Clever tpope
nnoremap <silent> <F9> :if &previewwindow<Bar>pclose<Bar>elseif exists(':Gstatus')<Bar>exe 'Gstatus'<Bar>else<Bar>ls<Bar>endif<CR>

" Easy paste mode toggling
set pastetoggle=<F6>

" Toggle search hilighting
map <silent> <F11> :set invhlsearch<CR>
imap <silent> <F11> <C-o>:set invhlsearch<CR>
vmap <silent> <F11> :<C-u>set invhlsearch<CR>gv

" It's a fast-moving world these days -- does your scrolling keep up?
noremap <C-y> 2<C-y>
noremap <C-e> 2<C-e>

" Don't use Ex mode, use Q for formatting
vnoremap Q gw
nnoremap Q gwap

" Yank from cursor to end of line, to be consistent with C and D.
nnoremap Y y$

" Select what was just pasted
noremap gV `[v`]

" Why so much hand lifting pain for command editing?
" Allow Emacs/readline-like keys.
cnoremap <C-A>      <Home>
cnoremap <C-B>      <Left>
cnoremap <C-E>      <End>
cnoremap <C-F>      <Right>
" Always use the smarter history prefix matching.
cnoremap <C-N>      <Down>
cnoremap <C-P>      <Up>
cnoremap <C-D>      <Del>
if has('mac') && has('gui_running')
  cnoremap <M-b>      <S-Left>
  cnoremap <M-f>      <S-Right>
  cnoremap <M-BS>     <C-W>
else
  cnoremap <ESC>b     <S-Left>
  cnoremap <ESC><C-B> <S-Left>
  cnoremap <ESC>f     <S-Right>
  cnoremap <ESC><C-F> <S-Right>
  cnoremap <ESC><BS>  <C-W>
  cnoremap <ESC><C-H> <C-W>
endif

" The defaults for these can be useful, keep them available. See also rsi.vim
cnoremap   <C-X><C-A> <C-A>
cnoremap   <C-X><C-D> <C-D>

" Default is Ctrl-F but we've just remapped it
set cedit=<C-y>

" Easy window split navigation
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l
map <C-h> <C-w>h

" Symmetry with tmux previous window binding
map <C-w>; <C-w>p

" Resizing, assuming left Option as Esc here
map <Esc>= <C-w>3+
map <Esc>- <C-w>3-
map <Esc>, <C-w>3<
map <Esc>. <C-w>3>

" Keep a block highlighted while shifting
vnoremap < <gv
vnoremap > >gv

" Line "bubbling" with the help of unimpaired.vim
nmap <C-UP> [e
nmap <C-DOWN> ]e
vmap <C-UP> [egv
vmap <C-DOWN> ]egv

" Toggle a window's height stickiness, so C-w = doesn't equalize it
nmap <leader>` :set invwinfixheight winfixheight?<CR>

" QuickLook the current file. With Brett Terpstra's awesome CSS fork of
" the MMD QuickLook plugin, this sure beats browser-based Markdown preview.
if has('mac')
  " Warning: OS X since Lion or so has fucked up qlmanage :-/
  nnoremap <Leader>ql :write<CR>:sil !qlmanage -p % >& /dev/null &<CR>:redraw!<CR>

  " dash.vim
  nmap <silent> <Leader>k <Plug>DashSearch
endif

" Terminal Function key hackery {{{
"
" Gross, but I'm tired of trying to get various terminal emulators to emit
" consistent fucking escape sequences. These, for now, are whatever iTerm2 in
" xterm-256color mode emits for function keys...
"
" http://stackoverflow.com/questions/3519532/mapping-function-keys-in-vim
" http://stackoverflow.com/questions/9950944/binding-special-keys-as-vim-shortcuts
"
" NOTE: Neovim ends a lot of the pain.
if has('mac') && (index(['alacritty', 'xterm-256color', 'screen-256color'], $TERM) >= 0)
  map <Esc>OP <F1>
  map <Esc>OQ <F2>
  map <Esc>OR <F3>
  map <Esc>OS <F4>
  map <Esc>[16~ <F5>
  map <Esc>[17~ <F6>
  map <Esc>[18~ <F7>
  map <Esc>[19~ <F8>
  map <Esc>[20~ <F9>
  map <Esc>[21~ <F10>
  map <Esc>[23~ <F11>
  map <Esc>[24~ <F12>

  imap <Esc>[17~ <F6>
  imap <Esc>[23~ <F11>
endif
"}}}

" Ellipsis. Mac Opt-; doesn't work in console.
if has('digraphs')
  digraph ./ 8230
endif

" Lotsa TextMate-inspired Mappings
runtime include/textmate-mappings.vim

" Language- and plugin-specific Preferences {{{1
" ----------------------------------------------

" For (sort of) modern standards in :TOhtml output
let g:html_use_css   = 1
let g:html_use_xhtml = 0

if has("autocmd")

  augroup FiletypeSets "{{{
    autocmd!
    autocmd BufNewFile,BufRead jquery.*.js set ft=javascript syntax=jquery
    autocmd BufNewFile,BufRead *.j2 set ft=jinja
    autocmd BufNewFile,BufRead *.mako set ft=mako

    " Remove for https://github.com/dag/vim-cabal -- it's WIP for
    " modularization of https://github.com/dag/vim2hs
    autocmd BufRead,BufNewFile *.cabal,*/.cabal/config,cabal{.sandbox,}.config setfiletype cabal
    autocmd BufRead cabal.sandbox.config setlocal readonly
  augroup END "}}}

  augroup OmniCompletion "{{{
    autocmd!
    autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
    autocmd FileType html setlocal omnifunc=htmlcomplete#CompleteTags
    autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
    autocmd FileType ruby setlocal omnifunc=rubycomplete#Complete
    autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags

    " Hide completion doc hints automatically after no longer needed
    autocmd InsertLeave *.py pclose
  augroup END "}}}

  " TODO: might soon want to start organizing this ballooning group of stuff
  " in after/ftplugin files :-)
  augroup FToptions "{{{
    autocmd!

    " Default text files to 78 characters.
    autocmd FileType text setlocal textwidth=78

    autocmd FileType html,xhtml,xml,htmldjango,htmljinja,eruby,mako,cucumber setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2
    autocmd FileType coffee,ruby,vim,yaml setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2
    autocmd FileType javascript setlocal expandtab shiftwidth=4 tabstop=4 softtabstop=4

    " Rails.vim defaults to 2 for traditional JS, I prefer 4
    autocmd User Rails.javascript* setlocal expandtab shiftwidth=4 tabstop=4 softtabstop=4
    autocmd User Rails.javascript.coffee* setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2

    " Comment continuation, joining
    autocmd FileType sh setlocal formatoptions+=roj
    " Auto-wrap comments to a nicely-readable width, assuming formatoptions+=c
    autocmd FileType go setlocal textwidth=80

    " Enhancements for whitespace-significant langs. Folding still a bit slow.
    autocmd FileType coffee,python BracelessEnable +indent +fold

    " Use Leader-. to write and execute. Prefer :Dispatch if shebanged though.
    autocmd FileType vim    map <buffer> <Leader>. :w!<CR>:source %<CR>
    autocmd FileType sh     map <buffer> <Leader>. :w!<CR>:!/bin/sh %<CR>
    autocmd FileType ruby   map <buffer> <Leader>. :w!<CR>:!ruby %<CR>
    autocmd FileType python map <buffer> <Leader>. :w!<CR>:!python %<CR>

    " Be trusting about Ruby code being evaluated for autocompletion...
    autocmd FileType ruby,eruby let g:rubycomplete_buffer_loading = 1
    autocmd FileType ruby,eruby let g:rubycomplete_rails = 1
    autocmd FileType ruby,eruby let g:rubycomplete_classes_in_global = 1

    " Easily lookup documentation
    " TODO: Perhaps make <Leader>k a convention for all language docs
    if has('mac')
      " The first keywords are custom search profiles that I've set up -- they
      " search the subsequent docsets as a group, ranked in the order shown here.
      autocmd User Rails :DashKeywords rr rails ruby
      autocmd User Rails.javascript* :DashKeywords jj js jquery
      autocmd User Rails.javascript.coffee* :DashKeywords cjj coffee js jquery

      autocmd FileType scala :DashKeywords scala akka play
    else
      autocmd FileType ruby noremap <buffer> <leader>rb :OpenURL http://apidock.com/ruby/<cword><CR>
      autocmd FileType ruby noremap <buffer> <leader>rr :OpenURL http://apidock.com/rails/<cword><CR>
    endif

    " Use fancier man.vim version instead of keywordprg
    autocmd FileType c,sh nnoremap K :Man <cword><CR>
    autocmd FileType vim setlocal keywordprg=:help

    autocmd FileType javascript let javascript_enable_domhtmlcss=1
    autocmd FileType xml let xml_use_xhtml = 1 " default xml to self-closing tags

    autocmd FileType vimwiki setlocal foldlevel=2 textwidth=78 linebreak
    autocmd FileType vimwiki map <buffer> <M-Space> <Plug>VimwikiToggleListItem
    autocmd FileType vimwiki map <buffer> <Leader>wg :VimwikiGoto<space>
    autocmd FileType vimwiki map <buffer> <Leader>w/ :VimwikiSearch<space>/

    autocmd FileType text,gitcommit,vimwiki setlocal spell
    autocmd FileType godoc,man,qf nnoremap <silent><buffer> q :q<CR>
    autocmd FileType man setlocal nocursorline nomodifiable
  augroup END "}}}

  " Fun with some goodies hidden in vim-git ftplugins. {{{
  augroup GitTricks
    autocmd!
    autocmd FileType gitrebase
          \ nnoremap <buffer> P :Pick<CR>   |
          \ nnoremap <buffer> S :Squash<CR> |
          \ nnoremap <buffer> E :Edit<CR>   |
          \ nnoremap <buffer> R :Reword<CR> |
          \ nnoremap <buffer> F :Fixup<CR>  |
          \ nnoremap <buffer> C :Cycle<CR>
  augroup END " }}}

  augroup Compilers "{{{
    " With regards to tpope. See his vimrc for more ideas.
    autocmd!

    " TODO: Focused tests a la :.Rake, try to use spin, etc. when available
    autocmd FileType cucumber let b:dispatch = 'cucumber %'
    autocmd FileType ruby
          \ let b:start = executable('pry') ? 'pry -r "%:p"' : 'irb -r "%:p"' |
          \ if expand('%') =~# '_test\.rb$' |
          \   let b:dispatch = 'testrb %' |
          \ elseif expand('%') =~# '_spec\.rb$' |
          \   let b:dispatch = 'rspec %' |
          \ else |
          \   let b:dispatch = 'ruby -wc %' |
          \ endif
    autocmd User Bundler
          \ if &makeprg !~# 'bundle' | setl makeprg^=bundle\ exec\  | endif
    autocmd FileType vim
          \ if exists(':Runtime') |
          \   let b:dispatch = ':Runtime' |
          \   let b:start = ':Runtime|PP' |
          \ else |
          \   let b:dispatch = ":unlet! g:loaded_{expand('%:t:r')}|source %" |
          \ endif
  augroup END "}}}

  augroup BaselineCompletion " {{{
    autocmd!
    autocmd FileType * if exists("+omnifunc") && &omnifunc == "" | setlocal omnifunc=syntaxcomplete#Complete | endif
    autocmd FileType * if exists("+completefunc") && &completefunc == "" | setlocal completefunc=syntaxcomplete#Complete | endif
  augroup END " }}}

  augroup Cursorline "{{{
    autocmd!

    " Turn on cursorline only on active window. Reduces clutter, easier to
    " find your place.
    "
    " cursorline is slow:
    "   http://briancarper.net/blog/590/cursorcolumn--cursorline-slowdown
    "   https://gist.github.com/pera/2624765
    "   https://code.google.com/p/vim/issues/detail?id=282
    autocmd WinEnter,BufRead * setlocal cursorline
    autocmd WinLeave * setlocal nocursorline
    autocmd InsertEnter * setlocal nocursorline
    autocmd InsertLeave * setlocal cursorline
  augroup END "}}}

  " Goyo: distraction-free writing {{{
  " This was VimRoom's default mapping:
  nnoremap <Leader>V :Goyo<CR>

  function! s:GoyoEnter()
    let s:goyo_scrolloff_save = &scrolloff
    set scrolloff=999
    set guioptions-=r

    if !empty($TMUX) && !has('gui_running')
      silent !tmux set status off
    endif
  endfunction

  function! s:GoyoLeave()
    let &scrolloff = s:goyo_scrolloff_save
    set guioptions+=r

    if !empty($TMUX) && !has('gui_running')
      silent !tmux set status on
    endif
  endfunction

  autocmd! User GoyoEnter
  autocmd! User GoyoLeave
  autocmd  User GoyoEnter nested call <SID>GoyoEnter()
  autocmd  User GoyoLeave nested call <SID>GoyoLeave()
  " }}}

  " Built-in cruft I never use -- don't bother loading it {{{
  let g:loaded_getscriptPlugin = 1
  let g:loaded_gzip            = 1
  let g:loaded_logipat         = 1
  let g:loaded_tarPlugin       = 1
  let g:loaded_tar             = 1
  let g:loaded_vimballPlugin   = 1
  let g:loaded_vimball         = 1
  let g:loaded_zipPlugin       = 1
  let g:loaded_zip             = 1
  " }}}

  " Haskell
  "
  " gf to buffer-local filetype settings at:
  "    ~/.vim/after/ftplugin/haskell.vim
  let g:necoghc_enable_detailed_browse = 1

  " Vimerl for Erlang
  "
  " Overwriting any existing buffer content is too surprising.
  let erlang_skel_replace = 0
  let erlang_skel_header = { "author": "Ches Martin" }

  " Python -- see also ~/.vim/after/ftplugin/python.vim {{{
  let python_highlight_all = 1

  " The way jedi-vim handles mappings with variable assignments is annoying --
  " like so many plugins, it ought to have a <Plug> map approach... I'm
  " leaving the (intrusive) default mappings for now, because it's a pain to
  " manually do all the other stuff that auto_initialization does if I turn it
  " off. The plugin at least needs an option to only skip its mappings.
  " let g:jedi#auto_initialization = 0

  let g:jedi#auto_close_doc = 0       " Overly jerky, spastic UI layout shifts
  let g:jedi#completions_enabled = 0  " We get jedi already with YouCompleteMe

  " vim-ipython - it's crusty but still fairly useful. Give it some love.
  let g:ipy_completefunc = 'none'  " Don't bother with completion, we have YCM/jedi
  let g:ipy_perform_mappings = 0   " Lots of intrusive mappings I'm not fond of
  " }}}

  " Go (golang) {{{
  " Tell vim-go to use goimports instead of gofmt on save
  let g:go_fmt_command = 'goimports'

  " Make things prettier -- unfortunately this is slow as balls
  " let g:go_highlight_functions = 1
  " let g:go_highlight_methods = 1
  " let g:go_highlight_structs = 1
  " let g:go_highlight_operators = 1
  " }}}

  " Rust {{{
  " For Racer to go to definition, autocomplete
  let $RUST_SRC_PATH = expand('~/src/rust/rust/src/')
  " }}}

  " Tagbar {{{
  runtime include/tagbar-types.vim
  let g:tagbar_autoclose = 1
  " }}}

  " NERDTree
  let NERDTreeWinPos          = 'right'
  let NERDTreeShowBookmarks   = 1
  let NERDTreeQuitOnOpen      = 1      " hide after opening a file
  let NERDTreeHijackNetrw     = 0      " I like netrw when I `:e somedir`
  let NERDTreeIgnore          = ['\.git', '\.hg', '\.svn', '\.DS_Store', '\.pyc']

  " NERDCommenter
  let NERDSpaceDelims         = 1      " use a space after comment chars
  let NERDDefaultAlign        = 'left'
  " Not cool when end-of-line comments break when uncommenting /* */ blocks:
  let NERDRemoveAltComs       = 0

  " TagmaTasks
  let g:TagmaTasksHeight   = 8
  let g:TagmaTasksTokens   = ['FIXME', 'TODO', 'NOTE', 'XXX', 'OPTIMIZE', 'PONY']
  let g:TagmaTasksJumpTask = 0
  " The plugin's jump mappings conflict with Unimpaired's tag nav
  let g:TagmaTasksJumpKeys = 0
  " Plugin is buggy, supposed to set this to empty but does so too late.
  let g:TagmaTasksRegexp = ''

  " Everything that seems more natural conflicts: <Leader>t with test runs;
  " <LocalLeader>t with type checks in typed langs; <C-t> with tag navigation.
  if has('mac') && has('gui_running')
    let g:TagmaTasksPrefix = '<M-t>'
  else
    let g:TagmaTasksPrefix = '<Esc>t'
  endif

  " Open the YankRing window
  if has('mac') && has('gui_running')
    nnoremap <silent> <M-v> :YRShow<CR>
  else
    " Console with Option as Escape
    nnoremap <silent> <Esc>v :YRShow<CR>
  endif

  let g:yankring_history_dir = s:my_backups_rootdir

  " Make sure YankRing plays nice with custom remapping.
  " See `:h yankring-custom-maps`
  function! YRRunAfterMaps()
    nnoremap <silent> Y   :<C-U>YRYankCount 'y$'<CR>
  endfunction

  let g:dbext_default_history_file = s:my_backups_rootdir . '/dbext_sql_history.txt'

  " Lusty Juggler buffer switcher
  let g:LustyJugglerShowKeys = 'a'
  let g:LustyJugglerSuppressRubyWarning = 1
  let g:LustyJugglerDefaultMappings = 0
  nmap <silent> <leader>b :LustyJuggler<CR>

  " Juggler is packaged with LustyExplorer, which I'm not interested in
  let g:loaded_lustyexplorer = 1

  " Gist {{{
  let g:gist_put_url_to_clipboard_after_post  = 1
  let g:gist_show_privates                    = 1
  let g:gist_post_private                     = 1
  " detect filetype if vim failed autodetection
  let g:gist_detect_filetype                  = 1
  " :w! updates a Gist, not plain :w
  let g:gist_update_on_write                  = 2
  if has('mac')
    let g:gist_clip_command                   = 'pbcopy'
  endif "}}}

  " Tslime {{{
  " Tslime provides a simple means of sending text to a tmux pane, most
  " usefully a REPL.
  "
  " There are some alternatives like Vimux, but I like the way Tslime prompts
  " for the window/pane to use, and allows reconfiguring it. This better suits
  " a larger pane for a REPL, where Vimux optimizes for running tests/builds
  " in a small pane that it creates. I prefer Dispatch for that. The new
  " vim-tmux-runner is worth a look.
  "
  " The Turbux test runner plugin can use Tslime, but its auto-detection of
  " backends gives precendence to Dispatch. This is normally desirable since
  " Dispatch can parse error output from async test/build runs into quickfix,
  " but if Tslime is preferable in some scenario, set:
  "
  "   let g:turbux_runner = 'tslime'
  "
  " A reminder for overriding Turbux's default test runner auto-selection:
  "
  "   let g:turbux_command_rspec = 'spin push'
  "
  vmap <C-c><C-c> <Plug>SendSelectionToTmux
  nmap <C-c><C-c> <Plug>NormalModeSendToTmux
  nmap <C-c>r <Plug>SetTmuxVars
  " }}}

  " Opt-in for fenced code block highlighting
  let g:markdown_fenced_languages = [
    \ 'coffee',
    \ 'js=javascript',
    \ 'python',
    \ 'ruby', 'erb=eruby',
    \ 'scala'
  \ ]

  " Vimwiki {{{

  " Using <Space>w to write files now, need to keep the old home for this.
  let g:vimwiki_map_prefix = ',w'

  " My custom functions below define a web link handler
  let g:vimwiki_menu      = 'Plugin.Vimwiki'
  let g:vimwiki_use_mouse = 1  " A rare case where I may actually use the mouse :-)
  let g:vimwiki_folding   = 1

  let main_wiki           = {}
  let main_wiki.path      = '~/src/vimwiki'
  let main_wiki.path_html = '~/src/vimwiki/html'
  let main_wiki.nested_syntaxes =
    \ {'python': 'python', 'ruby': 'ruby', 'sh': 'sh', 'vimscript': 'vim'}

  " 'diary' makes me feel like a teenage girl
  let main_wiki.diary_rel_path = 'journal/'
  let main_wiki.diary_index    = 'journal'
  let main_wiki.diary_header   = 'Journal'

  let g:vimwiki_list      = [main_wiki]
  " }}}

  if has('mac')
    " Map vim filetypes to Dash search keywords
    let g:dash_map = {
      \ 'python' : 'py',
      \ 'javascript' : 'js'
    \ }

    " netrw won't delete non-empty directories by default, which is annoying
    " TODO: Safe Linux equivalent to trash since this can be easy to fat-finger
    if executable('trash')
      let g:netrw_localrmdir = 'trash'  " brew install trash
    endif
  endif

  " Merlin - Semantic completion for OCaml
  "
  " Installed along with its server runtime through OPAM, so the Vim plugin is
  " loaded by giving NeoBundle a local path -- see:
  "
  "   https://github.com/the-lambda-church/merlin/wiki/vim-from-scratch
  "
  if neobundle#is_installed('merlin')
    let g:syntastic_ocaml_checkers = ['merlin']

    " Semantic text objects based on AST
    let g:merlin_textobject_grow   = 'm'
    let g:merlin_textobject_shrink = 'M'

    " See the above TODO for java patterns
    " let g:neocomplete#force_omni_input_patterns.ocaml = '[^. *\t]\.\w*\|\h\w*|#'
  endif
endif " has("autocmd")

" Plugin Mappings {{{2

" Ack Search
map <Leader>a  :Ack! ''<Left>
map <Leader>A  :AckWindow! ''<Left>
map <Leader>n  :AckFromSearch!<CR>
" Mnemonic: helpgrep, but consider making this a prefix for Lawrencium...
map <Leader>hg :AckHelp! ''<Left>

let g:ackhighlight = 1

" If The Silver Searcher is available, use it.
if executable('ag')
  let g:ackprg = 'ag --vimgrep'

  " ag is fast enough to just eschew caching altogether. Hot damn.
  let g:ctrlp_user_command = 'ag --nocolor -g "" %s'
  let g:ctrlp_use_caching = 0
elseif executable('ack')
  let g:ctrlp_user_command = 'ack -k --nocolor -g "" %s'
endif

" Airline status bar {{{
" TODO: detect availability
let g:airline_powerline_fonts = 1

if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif
let g:airline_symbols.maxlinenr = ''  " Default hamburger is clutter.

let g:airline#extensions#hunks#enabled = 0
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#show_buffers = 0
let g:airline#extensions#tabline#show_splits = 0
let g:airline#extensions#tabline#show_tab_type = 0
let g:airline#extensions#tabline#tab_min_count = 2
let g:airline#extensions#tabline#ignore_bufadd_pat = '\c\vgundo|tagbar|nerd_tree'

" Trailing whitespace bugs me alright, but not this much.
let g:airline#extensions#whitespace#enabled = 0

" let g:airline#extensions#tmuxline#enabled = 1
" let g:airline#extensions#tmuxline#snapshot_file = "~/.tmux/airline-colors.conf"

if !has('gui_running')
  " Many unfortunately look poor in the console, molokai almost works
  let g:airline_theme = 'base16'
endif " }}}

" AutoPairs
" Meta mappings use Esc for comfortable Option-as-Esc in iTerm
if !has('gui_running')
  let g:AutoPairsShortcutToggle = '<Esc>p'
  let g:AutoPairsShortcutFastWrap = '<Esc>e'
  let g:AutoPairsShortcutJump = '<Esc>n'
  let g:AutoPairsShortcutBackInsert = '<Esc>b'
endif

" localvimrc - https://github.com/embear/vim-localvimrc
let g:localvimrc_sandbox    = 0  " We ask before loading, this is too restrictive
let g:localvimrc_ask        = 1  " Default, but be paranoid since we don't sandbox
let g:localvimrc_persistent = 2  " Always store/restore decisions

let g:localvimrc_persistence_file = $MYVIMRUNTIME . '/localvimrc_persistent'

" Set up neocomplete/neocomplcache, instead of YouCompleteMe.
" runtime include/neocompl.vim

if has('python')
  " UltiSnips
  let g:UltiSnipsExpandTrigger       = "<tab>"
  let g:UltiSnipsJumpForwardTrigger  = "<tab>"
  let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"
  let g:UltiSnipsEditSplit           = "horizontal"

  " YouCompleteMe {{{
  " Wipe out the default Tab completions to stay out of the way of UltiSnips--
  " it's comfortable to just use Ctrl-n for YCM completion. Tried hacks for
  " overloading Tab and it wasn't worth it.
  let g:ycm_key_list_select_completion   = []
  let g:ycm_key_list_previous_completion = []

  " These extend the plugin's defaults. Values can be Python regexen.
  let g:ycm_semantic_triggers = {}
  let g:ycm_semantic_triggers.haskell  = ['.']
  let g:ycm_semantic_triggers.markdown = ['<']    " Using HTML omnicompletion
  let g:ycm_semantic_triggers.vimwiki  = ['[[']
  let g:ycm_semantic_triggers.php = ['->', '::', '(', 'use ', 'namespace ', '\']

  " Enable using tags. Off by default "because it's slow if your tags file is
  " on a network directory". lolwut.
  let g:ycm_collect_identifiers_from_tags_files = 1

  " If MacVim is installed from downloaded binary instead of built with
  " Homebrew Python installed, you might need to `brew unlink python` when
  " building YCM, and then set this:
  " let g:ycm_path_to_python_interpreter = '/usr/bin/python'
  " }}}

  " Gundo
  nnoremap <F7> :GundoToggle<CR>
  let g:gundo_preview_bottom = 1     " force wide window across bottom

  " Emmet, formerly Zen Coding
  "
  " Don't install in every filetype :-/
  " Might need to add stuff like ERB, or use composites.
  let g:user_emmet_install_global = 0
  autocmd FileType html,css EmmetInstall

  " The default mapping prefix conflicts with scrolling (<C-y>)...
  let g:user_emmet_leader_key='<C-m>'

else

  let g:gundo_disable = 1

endif

" VCSCommand {{{3

" The defaults (prefix of <leader>c) conflict with NERDCommenter, and
" I don't really like them anyway...
" FIXME: the bang commands don't work
map <Leader>va :VCSAdd<CR>
" 'Blame' is the most natural mnemonic for me
map <Leader>vb :VCSAnnotate<CR>
map <Leader>vB :VCSAnnotate!<CR>
map <Leader>vc :VCSCommit<CR>
map <Leader>vD :VCSDelete<CR>
map <Leader>vd :VCSDiff<CR>
map <Leader>vv :VCSVimDiff<CR>
" Close VCS scratch buffer and return - mnemonic: eXit :-)
map <silent> <Leader>vx :VCSGotoOriginal!<CR>
map <Leader>vl :VCSLog<CR>
" I think I'm far too likely to try 'r' for 'remove' all the time...
map <Leader>vR :VCSReview<CR>
map <Leader>vs :VCSStatus<CR>

" Fugitive {{{
noremap <C-g>s :Gstatus<CR>
noremap <C-g>c :Gcommit<CR>
noremap <C-g>d :Gdiff<CR>
noremap <C-g>D :Gvdiff<CR>
noremap <C-g>l :Glog<CR>
noremap <C-g>w :Gwrite<CR>
noremap <C-g>b :Gblame<CR>

noremap <C-g>ap :GstageFile<CR>

" Run any git command with output going to a git-smart temp buffer.
" Good for `diff` just to see unified instead of vimdiff, `show`, stash, etc.
noremap <C-g>gs :Gsplit!<Space>
noremap <C-g>gv :Gvsplit!<Space>

" Selectively restored in ~/.vim/after/ftplugin/gitv.vim
let g:Gitv_DoNotMapCtrlKey = 1

noremap <C-g>v :Gitv<CR>
noremap <C-g>V :Gitv!<CR>

" Open a new tab for current file in :Gdiff mode -- `dp` puts hunks to the
" index, i.e. it's basically `git add --patch`.
"
" Original buffer is left in leftmost window, so move there and remove it,
" leaving only the side-by-side diff.
"
" Originally snagged this from Gary Bernhardt, I think, with tweaks.
"
" TODO: take a file as optional argument, support file of current line in
" :Gstatus buffer
command! -bar GstageFile tabedit % | vsplit | Gvdiff | wincmd t | wincmd q

" Simple variation with status window at bottom, so you can easily `dv`
" other files from there to diff and stage them.
command! -bar Gstage GstageFile | Gstatus | wincmd J
" }}}

" Signify - VCS changes in the gutter with signs
let g:signify_vcs_list = ['git', 'hg', 'bzr']

" Sessions

" Commands namespaced under Session for more consistent recall/completion
let g:session_command_aliases = 1

" Specky - RSpec plugin {{{3

let g:speckyBannerKey = "<C-S>b"
let g:speckyQuoteSwitcherKey = "<C-S>'"
let g:speckyRunRdocKey = "<C-S>r"
let g:speckySpecSwitcherKey = "<C-S>x"
let g:speckyRunSpecKey = "<C-S>s"
"let g:speckyRunSpecCmd = "spec -fs"
let g:speckyRunRdocCmd = "qri -f plain"
let g:speckyWindowType = 1      " Horizontal split

" SplitJoin {{{3
let g:splitjoin_normalize_whitespace = 1
let g:splitjoin_align = 1

" Functions {{{1
" --------------

" Preserve -- restore cursor position after an operation {{{
"
" Generalized function to execute the given command while preserving cursor
" position, etc. Hat tip: http://vimcasts.org/episodes/tidying-whitespace/
" TODO: cleanse jumplist?
function! Preserve(command)
  " Preserve cursor position and last search
  let _s=@/
  let l = line(".")
  let c = col(".")
  " Do the damn thang
  execute a:command
  " Restore the saved bits
  let @/=_s
  call cursor(l, c)
endfunction " }}}

function! StripTrailingWhitespace()  " {{{
  " TODO: support ranges
  call Preserve("%s/\\s\\+$//e")
endfunction " }}}

function! ReformatFile()  " {{{
  call Preserve("normal gg=G")
endfunction " }}}

nmap _$ :call StripTrailingWhitespace()<CR>
nmap _= :call ReformatFile()<CR>

" OpenURL {{{
" Rails.vim and others call this by naming convention for various
" browser-opening functions
function! OpenURL(url)
  if has('mac')
    let g:browser = 'open '
  endif
  exec 'silent !'.g:browser.' '.a:url
endfunction " }}}

" open URL under cursor in browser
nnoremap gb :OpenURL <cfile><CR>
nnoremap gG :OpenURL http://www.google.com/search?q=<cword><CR>
nnoremap gW :OpenURL http://en.wikipedia.org/wiki/Special:Search?search=<cword><CR>

function! VimwikiWeblinkHandler(weblink) " {{{
  call OpenURL(a:weblink)
endfunction " }}}

" Quickfix utilities {{{2
" Courtesy of Dr. Mike Henry's vimfiles:
" https://github.com/drmikehenry/vimfiles
" See also: https://github.com/Valloric/ListToggle

" Return 1 if current window is the QuickFix window.
function! IsQuickFixWin()
  if &buftype == "quickfix"
    " This is either a QuickFix window or a Location List window.
    " Try to open a location list; if this window *is* a location list,
    " then this will succeed and the focus will stay on this window.
    " If this is a QuickFix window, there will be an exception and the
    " focus will stay on this window.
    try
      lopen
    catch /E776:/
      " This was a QuickFix window.
      return 1
    endtry
  endif
  return 0
endfunction

" Toggle quickfix window.
function! QuickFixWinToggle()
  let numOpenWindows = winnr("$")
  " Move to previous window before closing QuickFix window.
  if IsQuickFixWin() | wincmd p | endif
  cclose
  " Window was already closed, so open it.
  if numOpenWindows == winnr("$") | copen | endif
endfunction

" Toggle location list window.
function! LocListWinToggle()
  let numOpenWindows = winnr("$")
  lclose
  " Window was already closed, so open it.
  if numOpenWindows == winnr("$") | lopen | endif
endfunction

" TODO: make sure <C-q> working with flow control disabled; maybe use Leader?
nnoremap <silent> <C-q><C-q> :call QuickFixWinToggle()<CR>
nnoremap <silent> <C-q><C-l> :call LocListWinToggle()<CR>

" Commands {{{1
" -------------

if has("eval")
  command! -nargs=1 OpenURL :call OpenURL(<q-args>)

  " Bits stolen shamelessly from tpope
  " :edit with filename:lineno style supported
  command! -bar -nargs=1 -complete=file E :exe "edit ".substitute(<q-args>,'\(.*\):\(\d\+\):\=$','+\2 \1','')
  command! -bar -nargs=0 SudoW   :setl nomod|silent exe 'write !sudo tee % >/dev/null'|let &mod = v:shell_error
  command! -bar -nargs=* -bang W :write<bang> <args>
  command! -bar -count=0 RFC     :e http://www.ietf.org/rfc/rfc<count>.txt|setl ro noma
  command! -bar -nargs=0 -bang Scratch :silent edit<bang> \[Scratch] |
        \ set buftype=nofile bufhidden=hide noswapfile buflisted
  command! -bar -nargs=* -bang -complete=file Rename :
        \ let v:errmsg = ""|
        \ saveas<bang> <args>|
        \ if v:errmsg == ""|
        \   call delete(expand("#"))|
        \ endif

  command! -bar -nargs=? -complete=help Vhelp vertical help <args>
  cabbrev vhelp Vhelp
  cabbrev vh Vhelp
endif "}}}

" vim:foldmethod=marker foldlevel=0 foldclose=all commentstring="%s
