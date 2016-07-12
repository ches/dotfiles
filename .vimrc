"
"                              ,---.
"     ,--.   ,--.,--.          |   |
"      \  `.'  / `--',--,--,--.|  .'      Ches Martin
"       \     /  ,--.|        ||  |       http://chesmart.in
"        \   /   |  ||  |  |  |`--'
"         `-'    `--'`--`--`--'.--.
"                              '--'
"

" When started as "evim", evim.vim will already have done these settings.
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
  " Use Vim sauce, you can't do anything fun in Vi mode.
  " This must be first, because it changes other options as a side effect.
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

" General options {{{1
" Miscellaneous and Display {{{2

" allow backspacing over everything in insert mode
set backspace=indent,eol,start

" One place for backup and swap files, but we'll try a project-local .backup
" directory first if it exists.
if !isdirectory($HOME . '/.autosave/vim/swap')
  silent !mkdir -p ~/.autosave/vim/swap > /dev/null 2>&1
endif

" // make swap files unique based on path
set directory=./.backup//,~/.autosave/vim/swap//

if has("vms")
  set nobackup      " do not keep a backup file, use versions instead
else
  set backup        " keep a backup file
  set backupdir=./.backup,~/.autosave/vim
endif

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

" General omnicompletion
set omnifunc=syntaxcomplete#Complete

" Basically the default statusline when ruler is enabled, with fugitive
set statusline=%<%f\ %h%m%r%{fugitive#statusline()}%=%-14.(%l,%c%V%)\ %P

" Ensure Airline is always on, instead of only appearing when there's a split.
set laststatus=2
set noshowmode
set ttimeoutlen=30

" automatically flush to disk when using :make, changing buffers, etc.
" Alternatively, set hidden to allow moving around and leaving dirty files be
"set autowrite
set hidden

" If file changed outside vim but not inside, just read it
set autoread

" use existing window if I try to open an already-open buffer
set switchbuf=useopen

" New h/v split window show up on bottom/right
set splitbelow splitright

" threshold for reporting number of lines changed
set report=0

" For (sort of) modern standards in :TOhtml output
let g:html_use_css   = 1
let g:html_use_xhtml = 0

" Don't use Ex mode, use Q for formatting
vnoremap Q gq
nnoremap Q gqap

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
    colorscheme base16-default
  endif
endif

if has("viminfo")
  " Allow some global variables to persist between sessions
  " Plugins sometimes use this to retain useful things
  " % saves and restores buffer list when started with no args
  set viminfo^=!,%
elseif has('shada')  " Neovim
  set shada^=!,%
endif

" Indentation {{{2

" use previous line's indentation
set autoindent

" true Tabs display as 8 columns in most tools, but that just looks too wide
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

" Use attractive characters to show tabs & trailing spaces
set listchars=tab:»·,trail:·,eol:¬,nbsp:␣

" Folding {{{2

set foldmethod=syntax   " try to fold in an intelligent manner based on ftplugins
set foldlevelstart=99   " default to all folds open when opening a buffer
set foldnestmax=4       " don't be absurd about how deeply to nest syntax folding
"set foldclose=all      " close a fold when I leave it
set foldopen-=block     " drives me nuts that moving with ] opens folds

" Autocommands {{{2
if has("autocmd")

  augroup BufActions
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
    endfunction
    "}}}

    " Easy helptags regeneration when editing my personal Vim notes
    autocmd BufRead ~/.vim/doc/my-notes.txt
          \ setlocal modifiable iskeyword=!-~,^*,^\|,^\",192-255 |
          \ map <buffer> <Leader><space> :w!<CR>:helptags ~/.vim/doc<CR>

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
  augroup END

endif " has("autocmd")

" General Mappings {{{1

" I'm drinkin' the comma-as-leader kool aid
let mapleader = ","
let maplocalleader = "\\"

" Edit vimrc. Use <leader><space> mapping (when active buffer) to source it.
nnoremap <leader>ev :split  $MYVIMRC<CR>
nnoremap <leader>eV :vsplit $MYVIMRC<CR>
nnoremap <leader>er :split  $MYVIMRUNTIME/<CR>
nnoremap <leader>eR :vsplit $MYVIMRUNTIME/<CR>
nnoremap <leader>en :split  ~/.vim/doc/my-notes.txt<CR>
nnoremap <leader>eN :vsplit ~/.vim/doc/my-notes.txt<CR>

" Search runtime files (plugins) -- warning: slow!
nnoremap <leader>eP :CtrlPRTS<CR>

" TODO: make this a command, something like this but proper completions:
" command -bar -nargs=? -complete=help Help help my-notes-<args>
nnoremap <leader>hh :help my-notes-

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
  nnoremap <Leader>ql :write<CR>:sil !qlmanage -p % >& /dev/null &<CR>:redraw!<CR>

  " dash.vim
  nmap <silent> <leader>k <Plug>DashSearch
endif

" Terminal Function key hackery {{{
"
" Gross, but I'm tired of trying to get various terminal emulators to emit
" consistent fucking escape sequences. These, for now, are whatever iTerm2 in
" xterm-256color mode emits for function keys...
"
" http://stackoverflow.com/questions/3519532/mapping-function-keys-in-vim
" http://stackoverflow.com/questions/9950944/binding-special-keys-as-vim-shortcuts
if has('mac') && ($TERM == 'xterm-256color' || $TERM == 'screen-256color')
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

" Lotsa TextMate-inspired Mappings
runtime include/textmate-mappings.vim

" Language- and plugin-specific Preferences {{{1
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
    autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
    autocmd FileType ruby setlocal omnifunc=rubycomplete#Complete
    autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
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

    " Comment continuation
    autocmd FileType sh setlocal formatoptions+=roj

    " Use leader+space to write and execute
    autocmd FileType vim map <buffer> <leader><space> :w!<cr>:source %<cr>
    autocmd FileType sh map <buffer> <leader><space> :w!<cr>:!/bin/sh %<cr>
    autocmd FileType ruby map <buffer> <leader><space> :w!<cr>:!ruby %<cr>
    autocmd FileType python map <buffer> <leader><space> :w!<cr>:!python %<cr>

    " Be trusting about Ruby code being evaluated for autocompletion
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

    " Make pydoc.vim's doc window close easier
    autocmd BufNewFile __doc__ nmap <silent> <buffer> q :q<CR>
    " autocmd FileType python setlocal cinwords=if,elif,else,for,while,try,except,finally,def,class,with

    autocmd FileType javascript let javascript_enable_domhtmlcss=1
    autocmd FileType xml let xml_use_xhtml = 1 " default xml to self-closing tags

    autocmd FileType vimwiki setlocal foldlevel=2 textwidth=78 linebreak
    autocmd FileType vimwiki map <buffer> <M-Space> <Plug>VimwikiToggleListItem
    autocmd FileType vimwiki map <buffer> <Leader>wg :VimwikiGoto<space>
    autocmd FileType vimwiki map <buffer> <Leader>w/ :VimwikiSearch<space>/

    autocmd FileType text,gitcommit,vimwiki setlocal spell
    autocmd FileType man,qf nnoremap <silent><buffer> q :q<CR>
  augroup END "}}}

  " Fun with some goodies hidden in vim-git ftplugins.
  augroup GitTricks
    autocmd!
    autocmd FileType gitrebase
          \ nnoremap <buffer> P :Pick<CR>   |
          \ nnoremap <buffer> S :Squash<CR> |
          \ nnoremap <buffer> E :Edit<CR>   |
          \ nnoremap <buffer> R :Reword<CR> |
          \ nnoremap <buffer> F :Fixup<CR>  |
          \ nnoremap <buffer> C :Cycle<CR>
  augroup END

  " With regards to tpope. See his vimrc for more ideas.
  " TODO: consolidate with dispatch.vim
  augroup Compilers "{{{
    autocmd!
    " TODO: Focused tests a la :.Rake, try to use spin, etc. when available
    autocmd FileType cucumber compiler cucumber | setl makeprg=cucumber\ \"%:p\"
    autocmd FileType ruby
          \ if expand('%') =~# '_test\.rb$' |
          \   compiler rubyunit | setl makeprg=testrb\ \"%:p\" |
          \ elseif expand('%') =~# '_spec\.rb$' |
          \   compiler rspec | setl makeprg=rspec\ \"%:p\" |
          \ else |
          \   compiler ruby | setl makeprg=ruby\ -wc\ \"%:p\" |
          \ endif
    autocmd User Bundler
          \ if &makeprg !~# 'bundle' | setl makeprg^=bundle\ exec\  | endif
  augroup END "}}}

  augroup Cursorline "{{{
    autocmd!

    " Turn on cursorline only on active window. Reduces clutter, easier to
    " find your place.
    "
    " cursorline is slow:
    "   http://briancarper.net/blog/590/cursorcolumn--cursorline-slowdown
    "   https://gist.github.com/pera/2624765
    "   https://code.google.com/p/vim/issues/detail?id=282
    autocmd WinLeave * setlocal nocursorline
    autocmd WinEnter,BufRead * setlocal cursorline
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

  let python_highlight_all = 1

  " Chapa provides nice mnemonic movement, selection
  " and commenting for Python and JavaScript.
  let g:chapa_default_mappings   = 1
  let g:chapa_no_repeat_mappings = 1

  " Rust {{{
  " For Racer to go to definition, autocomplete
  let $RUST_SRC_PATH = expand('~/src/rust/rust/src/')
  " }}}

  " Tagbar {{{
  nnoremap <Leader>. :TagbarToggle<CR>

  runtime include/tagbar-types.vim
  let g:tagbar_autoclose = 1
  " }}}

  " NERDTree
  let NERDTreeWinPos          = 'right'
  let NERDTreeShowBookmarks   = 1
  let NERDTreeQuitOnOpen      = 1      " hide after opening a file
  let NERDTreeHijackNetrw     = 0      " I like netrw when I `:e somedir`
  let NERDTreeIgnore          = ['\.git','\.hg','\.svn','\.DS_Store']

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

  let g:yankring_history_dir = '$HOME/.autosave/vim'

  " Make sure YankRing plays nice with custom remapping.
  " See `:h yankring-custom-maps`
  function! YRRunAfterMaps()
    nnoremap <silent> Y   :<C-U>YRYankCount 'y$'<CR>
  endfunction

  let g:dbext_default_history_file = '$HOME/.autosave/vim'

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

  " Opt-in for fenced code block highlighting
  let g:markdown_fenced_languages = [
    \ 'coffee',
    \ 'js=javascript',
    \ 'python',
    \ 'ruby', 'erb=eruby',
    \ 'scala'
  \ ]

  " Vimwiki {{{
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

    " opam install ocp-indent
    " autocmd FileType ocaml source g:opamshare . "/typerex/ocp-indent/ocp-indent.vim"
  endif
endif " has("autocmd")

" Plugin Mappings {{{2

" Ack Search
map <Leader>a  :Ack! ''<Left>
map <Leader>A  :AckWindow! ''<Left>
map <Leader>n  :AckFromSearch!<CR>
map <Leader>hg :AckHelp! ''<Left>

let g:ackhighlight = 1

" If The Silver Searcher is available, use it.
"
" TODO: maybe use as 'grepprg' too
if executable('ag')
  let g:ackprg = 'ag --vimgrep'

  " ag is fast enough to just eschew caching altogether. Hot damn.
  let g:ctrlp_user_command = 'ag --nocolor -g "" %s'
  let g:ctrlp_use_caching = 0
elseif executable('ack')
  let g:ctrlp_user_command = 'ack -k --nocolor -g "" %s'
endif

"
" Airline status bar
"

" TODO: detect availability
let g:airline_powerline_fonts = 1
let g:airline#extensions#hunks#enabled = 0
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#show_buffers = 0
let g:airline#extensions#tabline#show_tab_type = 0
let g:airline#extensions#tabline#tab_min_count = 2

" Trailing whitespace bugs me alright, but not this much.
let g:airline#extensions#whitespace#enabled = 0

" let g:airline#extensions#tmuxline#enabled = 1
" let g:airline#extensions#tmuxline#snapshot_file = "~/.tmux/airline-colors.conf"

if !has('gui_running')
  " Many unfortunately look poor in the console, molokai almost works
  let g:airline_theme = 'base16'
endif

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

" NERD tree - double-leader
map <Leader><Leader> :NERDTreeToggle<cr>

" Mnemonic: [f]iles or [f]unctions, with a shared key. <Leader>b is LustyJuggler.
nnoremap <Leader>f      :CtrlP<CR>
nnoremap <Leader>F      :CtrlPBuffer<CR>
nnoremap <LocalLeader>f :CtrlPBufTag<CR>
nnoremap <Leader><C-f>  :CtrlPTag<CR>

" Ready for tab-completion of named Tabular patterns
" Choosing 'gq' since it's similar function to the format command
map <Leader>gq :Tabularize<space>
map <Leader>q= :Tabularize assignments<CR>

" Set up neocomplete/neocomplcache, instead of YouCompleteMe.
" runtime include/neocompl.vim

if has('python')
  " UltiSnips
  let g:UltiSnipsExpandTrigger       = "<tab>"
  let g:UltiSnipsJumpForwardTrigger  = "<tab>"
  let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"
  let g:UltiSnipsEditSplit           = "horizontal"

  " YouCompleteMe
  " Wipe out the default Tab completions to stay out of the way of UltiSnips--
  " it's comfortable to just use Ctrl-n for YCM completion. Tried hacks for
  " overloading Tab and it wasn't worth it.
  let g:ycm_key_list_select_completion   = []
  let g:ycm_key_list_previous_completion = []

  let g:ycm_semantic_triggers = {}
  let g:ycm_semantic_triggers.haskell = ['.']

  " Enable using tags. Off by default "because it's slow if your tags file is
  " on a network directory". lolwut.
  let g:ycm_collect_identifiers_from_tags_files = 1

  " If MacVim is installed from downloaded binary instead of built with
  " Homebrew Python installed, you might need to `brew unlink python` when
  " building YCM, and then set this:
  " let g:ycm_path_to_python_interpreter = '/usr/bin/python'

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

" Custom Functions {{{1

" Generalized function to execute the given command while preserving cursor
" position, etc. Hat tip: http://vimcasts.org/episodes/tidying-whitespace/
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
endfunction

function! StripTrailingWhitespace()
  call Preserve("%s/\\s\\+$//e")
endfunction

function! ReformatFile()
  call Preserve("normal gg=G")
endfunction

nmap _$ :call StripTrailingWhitespace()<CR>
nmap _= :call ReformatFile()<CR>

" Rails.vim and others call this by naming convention for various
" browser-opening functions
function! OpenURL(url)
  if has('mac')
    let g:browser = 'open '
  endif
  exec 'silent !'.g:browser.' '.a:url
endfunction
command! -nargs=1 OpenURL :call OpenURL(<q-args>)

nnoremap gG :OpenURL http://www.google.com/search?q=<cword><CR>
nnoremap gW :OpenURL http://en.wikipedia.org/wiki/Special:Search?search=<cword><CR>

function! VimwikiWeblinkHandler(weblink)
  call OpenURL(a:weblink)
endfunction

" Commands {{{1

" Stolen shamelessly from tpope
if has("eval")
  command! -bar -nargs=1 -complete=file E :exe "edit ".substitute(<q-args>,'\(.*\):\(\d\+\):\=$','+\2 \1','')
  command! -bar -nargs=0 SudoW   :setl nomod|silent exe 'write !sudo tee % >/dev/null'|let &mod = v:shell_error
  command! -bar -nargs=* -bang W :write<bang> <args>
  command! -bar -nargs=0 -bang Scratch :silent edit<bang> \[Scratch]|set buftype=nofile bufhidden=hide noswapfile buflisted
  command! -bar -count=0 RFC     :e http://www.ietf.org/rfc/rfc<count>.txt|setl ro noma
  command! -bar -nargs=* -bang -complete=file Rename :
        \ let v:errmsg = ""|
        \ saveas<bang> <args>|
        \ if v:errmsg == ""|
        \   call delete(expand("#"))|
        \ endif
endif

" vim:foldmethod=marker commentstring="%s

