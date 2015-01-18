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

if has('vim_starting')
  " Use Vim sauce, you can't do anything fun in Vi mode.
  " This must be first, because it changes other options as a side effect.
  set nocompatible
  set runtimepath+=~/.vim/bundle/neobundle.vim
endif

" Register and load plugins.
runtime include/bundles.vim

" Built-ins
runtime macros/editexisting.vim  " Bring forward existing session w/ open file
runtime macros/matchit.vim       " More powerful % for if/fi, HTML tags, etc.
runtime ftplugin/man.vim         " Sweet :Man command opens pages in split

" Allow plugins to work their magic.
filetype plugin indent on

" Check and prompt for any plugins pending installation.
NeoBundleCheck

" General options {{{1
" Miscellaneous and Display {{{2

" allow backspacing over everything in insert mode
set backspace=indent,eol,start

if has("vms")
  set nobackup                  " do not keep a backup file, use versions instead
else
  set backup                    " keep a backup file
  set backupdir=~/.autosave/vim " leave all the droppings in one place
endif

set history=500     " keep more command line history
set ruler           " show the cursor position all the time
set showcmd         " display commands as they're being entered
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

" Silence CSApprox's gripes if running a vim without gui support
if !has('gui')
  let g:CSApprox_loaded = 1
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
    colorscheme customtwilight
  endif
endif

" Only do this part when compiled with support for autocommands.
if has("autocmd")

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

set foldmethod=syntax   " try to fold in an intelligent manner based on ftplugins
set foldlevelstart=99   " default to all folds open when opening a buffer
set foldnestmax=4       " don't be absurd about how deeply to nest syntax folding
"set foldclose=all      " close a fold when I leave it
set foldopen-=block     " drives me nuts that moving with ] opens folds

" Colors {{{2

" These might be desired depending on colorscheme
"highlight LineNr  term=underline    ctermfg=grey    guifg=grey
"highlight CursorLine    guibg=Grey10
" No hideous pink default autocomplete menu
"highlight PMenu gui=bold guibg=#CECECE guifg=#444444

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
          exe "normal g`\""
          normal! zz
        endif
      end
    endfunction
    "}}}

    " Skeletons {{{
    autocmd BufNewFile build.sbt silent 0read ~/.vim/skeleton/build.sbt| normal ggf"
    autocmd BufNewFile .lvimrc silent 0read ~/.vim/skeleton/lvimrc.vim | normal }j
    autocmd BufNewFile *.py silent 0read ~/.vim/skeleton/skeleton.py   | normal G
    autocmd BufNewFile *.sh silent 0read ~/.vim/skeleton/skeleton.sh   | normal G
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

    " Automatically distribute my vimrc to the servers I use {{{
    "autocmd BufWritePost ~/.vimrc !scp ~/.vimrc valleyofwind.dyndns.org:.
    "autocmd BufWritePost ~/.vim/skeletons/* !scp % valleyofwind.dyndns.org:.vim/skeletons/
    "}}}

  augroup END

endif " has("autocmd")

" General Mappings {{{1

" I'm drinkin' the comma-as-leader kool aid
let mapleader = ","
let maplocalleader = "\\"

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
  nnoremap <Leader>qlk :sil !killall qlmanage >& /dev/null<CR>

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
    " Keep the multiplying zombie virus-infected fugitive buffer hoard at bay
    autocmd BufReadPost fugitive://* set bufhidden=delete
  augroup END "}}}

  augroup OmniCompletion "{{{
    autocmd!
    autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
    autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
    autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
    autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
    autocmd FileType ruby setlocal omnifunc=rubycomplete#Complete
    autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
  augroup END "}}}

  " TODO: might soon want to start organizing this ballooning group of stuff
  " in after/ftplugin files :-)
  augroup FToptions "{{{
    autocmd!
    autocmd FileType html,xhtml,xml,htmldjango,htmljinja,eruby,mako,cucumber setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2
    autocmd FileType coffee,ruby,scala,vim,yaml setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2
    autocmd FileType javascript setlocal expandtab shiftwidth=4 tabstop=4 softtabstop=4

    " Rails.vim defaults to 2 for traditional JS, I prefer 4
    autocmd User Rails.javascript* setlocal expandtab shiftwidth=4 tabstop=4 softtabstop=4
    autocmd User Rails.javascript.coffee* setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2

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
    autocmd FileType python nnoremap <silent> <buffer> <F5> :call Pep8()<CR>
    " autocmd FileType python setlocal cinwords=if,elif,else,for,while,try,except,finally,def,class,with

    autocmd FileType javascript let javascript_enable_domhtmlcss=1
    autocmd FileType xml let xml_use_xhtml = 1 " default xml to self-closing tags

    autocmd FileType markdown nnoremap <buffer> <leader>1 yypVr=
    autocmd FileType markdown nnoremap <buffer> <leader>2 yypVr-
    autocmd FileType markdown setlocal linebreak

    autocmd FileType vimwiki setlocal foldlevel=2 textwidth=78 linebreak
    autocmd FileType vimwiki map <buffer> <M-Space> <Plug>VimwikiToggleListItem
    autocmd FileType vimwiki map <buffer> <Leader>wg :VimwikiGoto<space>
    autocmd FileType vimwiki map <buffer> <Leader>w/ :VimwikiSearch<space>/

    autocmd FileType text,markdown,gitcommit,vimwiki setlocal spell
    autocmd FileType help,man,qf nnoremap <silent><buffer> q :q<CR>
  augroup END "}}}

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

  augroup Cursorline
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
  augroup END


  " Vimerl for Erlang
  "
  " Overwriting any existing buffer content is too surprising.
  let erlang_skel_replace = 0
  let erlang_skel_header = { "author": "Ches Martin" }

  let python_highlight_all = 1

  " TagBar
  runtime include/tagbar-types.vim
  let g:tagbar_autoclose = 1

  " Shifted version of NERDTree toggle
  map <Leader>< :TagbarToggle<CR>

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
  " Defaults to <Leader>t, which would make CommandT slow
  let g:TagmaTasksPrefix = '\t'
  " Plugin is buggy, supposed to set this to empty but does so too late.
  let g:TagmaTasksRegexp = ''

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
  nmap <silent> <leader>b :LustyJuggler<CR>

  " Gist
  let g:gist_put_url_to_clipboard_after_post  = 1
  let g:gist_show_privates                    = 1
  let g:gist_post_private                     = 1
  " detect filetype if vim failed autodetection
  let g:gist_detect_filetype                  = 1
  " :w! updates a Gist, not plain :w
  let g:gist_update_on_write                  = 2
  if has('mac')
    let g:gist_clip_command                   = 'pbcopy'
  endif

  " Tslime / Turbux combo for REPL interaction/running tests in tmux panes. No
  " quickfix list, but nice when tests are out-of-process anyway like spin or
  " spork. Note: Vimux is nice too, but I like tslime's window/pane control.
  " To customize per-project at runtime:
  "   let g:turbux_command_rspec = 'spin push'
  "
  " TODO: Turbux can use Dispatch now, maybe drop tslime?
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
  " not managed via Vundle currently -- see:
  "
  "   https://github.com/the-lambda-church/merlin
  "
  " Reminder to re-index the docs after upgrades:
  "
  "   :execute "helptags " . g:opamshare . "/merlin/vim/doc"
  if executable('ocamlmerlin') && has('python')
    let g:opamshare = substitute(system('opam config var share'), '\n$', '', '''')
    execute "set rtp+=" . g:opamshare . "/merlin/vim"

    let g:syntastic_ocaml_checkers = ['merlin']

    " See the above TODO for java patterns
    " let g:neocomplete#force_omni_input_patterns.ocaml = '[^. *\t]\.\w*\|\h\w*|#'

    " opam install ocp-indent
    " autocmd FileType ocaml source g:opamshare . "/typerex/ocp-indent/ocp-indent.vim"
  endif
endif " has("autocmd")

" Plugin Mappings {{{2

" Ack Search
map <Leader>a :Ack!<space>
map <Leader>A :AckWindow!<space>
map <Leader>n :AckFromSearch!<CR>

let g:ackhighlight = 1

" If The Silver Searcher is available, use it.
"
" The --noheading option to ag should more probably be ack's -H, it currently
" results in some extra newlines but Vim quickfix deals with it well enough.
" See:
"
"   https://github.com/ggreer/the_silver_searcher/issues/361
if executable('ag')
  let g:ackprg = 'ag --nogroup --nocolor --noheading --column'

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
  let g:airline_theme = 'ubaryd'
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

let g:localvimrc_persistence_file = expand('$HOME') . '/.backup/vim/localvimrc_persistent'

" NERD tree - double-leader
map <Leader><Leader> :NERDTreeToggle<cr>

" Mnemonic: [f]iles, with a shared key. I use <Leader>b for LustyJuggler.
nnoremap <Leader>f :CtrlP<CR>
nnoremap <Leader>F :CtrlPBuffer<CR>
nnoremap <Leader><C-f> :CtrlPTag<CR>

" Ready for tab-completion of named Tabular patterns
" Choosing 'gq' since it's similar function to the format command
map <Leader>gq :Tabularize<space>

if has('python')
  " UltiSnips
  let g:UltiSnipsExpandTrigger       = "<tab>"
  let g:UltiSnipsJumpForwardTrigger  = "<tab>"
  let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"
  let g:UltiSnipsEditSplit           = "horizontal"
  nmap <Leader>rs :py UltiSnips_Manager.reset()<CR>

  " YouCompleteMe
  " Wipe out the default Tab completions to stay out of the way of UltiSnips--
  " it's comfortable to just use Ctrl-n for YCM completion. Tried hacks for
  " overloading Tab and it wasn't worth it.
  let g:ycm_key_list_select_completion   = []
  let g:ycm_key_list_previous_completion = []

  " Enable using tags. Off by default "because it's slow if your tags file is
  " on a network directory". lolwut.
  let g:ycm_collect_identifiers_from_tags_files = 1

  " I haven't built YCM's Clang magic initially, big slow download/build
  let g:ycm_register_as_syntastic_checker = 0
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

" Fugitive
noremap <C-g>s :Gstatus<CR>
noremap <C-g>c :Gcommit<CR>
noremap <C-g>d :Gdiff<CR>
noremap <C-g>l :Glog<CR>
noremap <C-g>w :Gwrite<CR>
noremap <C-g>b :Gblame<CR>

noremap <C-g>v :Gitv<CR>
noremap <C-g>V :Gitv!<CR>

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

