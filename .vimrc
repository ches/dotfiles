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
set gdefault        " Line-global substitution by default, usually what you want
set scrolloff=3     " Keep some context visible when scrolling
set sidescrolloff=4
set wildmenu        " Modern completion menu
set number          " line numbers
set numberwidth=5   " a little bit of buffer is prettier

" wildmenu does shell-style completion AND tab-through
set wildmode=list:longest,full

" Ignore some extensions when tab-completing
set wildignore=*.swp,*.bak,*.pyc,*.o,*.class

" Only insert up to longest common autocomplete match
set completeopt+=longest

" Basically the default statusline when ruler is enabled, with fugitive
set statusline=%<%f\ %h%m%r%{fugitive#statusline()}%=%-14.(%l,%c%V%)\ %P

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
  colorscheme customtwilight  " last used: oceanblack
endif

" Only do this part when compiled with support for autocommands.
if has("autocmd")

  " Vundle has similar particularities about filetype initialization as
  " pathogen does
  filetype off
  runtime! include/bundles.vim

  " Enable file type detection, letting plugins, autocmds and such do all
  " their magic for custom language-dependent settings.
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

" Get a sweet :Man command to open pages in split
runtime ftplugin/man.vim
nmap K :Man <cword><CR>

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
    autocmd BufNewFile *.py silent 0read ~/.vim/skeleton/skeleton.py   | normal G
    autocmd BufNewFile *.sh silent 0read ~/.vim/skeleton/skeleton.sh   | normal G
    "autocmd BufNewFile *.pl silent 0read ~/.vim/skeleton/perl.pl     | normal G
    "autocmd BufNewFile *.t  silent 0read ~/.vim/skeleton/perl-test.t | normal G
    "autocmd BufNewFile *.c  silent 0read ~/.vim/skeleton/c.c         | normal 4j$
    "autocmd BufNewFile *.hs silent 0read ~/.vim/skeleton/haskell.hs  | normal Gk$
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
" Remappings {{{1

" I'm drinkin' the comma-as-leader kool aid
let mapleader = ","

" Omni completion shortcut
imap <M-space> <C-x><C-o><C-p>

" See tpope's wicked Run() function -- I'd like to cook up something
" similar ror ruby, pyflakes/pep8, etc.
map <F2> :cprev<CR>
map <F3> :wa<Bar>make<CR>
map <F4> :cnext<CR>

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

  augroup FiletypeSets "{{{
    autocmd!
    autocmd BufNewFile,BufRead jquery.*.js set ft=javascript syntax=jquery
    autocmd BufNewFile,BufRead *.mako set ft=mako
    autocmd BufNewFile,BufRead Rakefile,Capfile,Gemfile,Vagrantfile set ft=ruby
    " Keep the multiplying zombie virus-infected fugitive buffer hoard at bay
    autocmd BufReadPost fugitive://* set bufhidden=delete
  augroup END "}}}

  " TODO: might soon want to start organizing this ballooning group of stuff
  " in after/ftplugin files :-)
  augroup FToptions "{{{
    autocmd!
    autocmd FileType html,xhtml,xml,htmldjango,htmljinja,eruby,mako,cucumber setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2
    autocmd FileType ruby,vim,yaml setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2
    autocmd FileType javascript setlocal expandtab shiftwidth=4 tabstop=4 softtabstop=4
    autocmd User Rails.javascript* setlocal expandtab shiftwidth=4 tabstop=4 softtabstop=4

    " Use leader+space to write and execute
    autocmd FileType vim map <buffer> <leader><space> :w!<cr>:source %<cr>
    autocmd FileType sh map <buffer> <leader><space> :w!<cr>:!/bin/sh %<cr>
    autocmd FileType ruby map <buffer> <leader><space> :w!<cr>:!ruby %<cr>
    autocmd FileType python map <buffer> <leader><space> :w!<cr>:!python %<cr>

    " Be trusting about Ruby code being evaluated for autocompletion
    autocmd FileType ruby,eruby let g:rubycomplete_buffer_loading = 1
    autocmd FileType ruby,eruby let g:rubycomplete_rails = 1
    autocmd FileType ruby,eruby let g:rubycomplete_classes_in_global = 1
    " Easily lookup documentation on apidock
    autocmd FileType ruby noremap <buffer> <leader>rb :OpenURL http://apidock.com/rails/<cword><CR>
    autocmd FileType ruby noremap <buffer> <leader>rr :OpenURL http://apidock.com/ruby/<cword><CR>

    autocmd FileType python nnoremap <silent> <buffer> K :call ShowPyDoc(expand("<cword>"), 1)<CR>
    autocmd FileType python nnoremap <silent> <buffer> <F5> :call Pep8()<CR>
    " autocmd FileType python setlocal cinwords=if,elif,else,for,while,try,except,finally,def,class,with

    autocmd FileType javascript let javascript_enable_domhtmlcss=1
    autocmd FileType xml let xml_use_xhtml = 1 " default xml to self-closing tags
    autocmd FileType vim setlocal keywordprg=:help

    autocmd FileType markdown nnoremap <buffer> <leader>1 yypVr=
    autocmd FileType markdown nnoremap <buffer> <leader>2 yypVr-

    autocmd FileType vimwiki setlocal foldlevel=2 textwidth=78 linebreak
    autocmd FileType vimwiki map <buffer> <M-Space> <Plug>VimwikiToggleListItem
    autocmd FileType vimwiki map <buffer> <Leader>wg :VimwikiGoto<space>
    autocmd FileType vimwiki map <buffer> <Leader>w/ :VimwikiSearch<space>/

    autocmd FileType text,markdown,gitcommit,vimwiki setlocal spell
    autocmd FileType help,man,qf nnoremap <silent><buffer> q :q<CR>
  augroup END "}}}

  let python_highlight_all = 1

  " Taglist
  let Tlist_Use_Right_Window        = 1
  let Tlist_GainFocus_On_ToggleOpen = 1
  let Tlist_Enable_Fold_Column      = 0
  let Tlist_File_Fold_Auto_Close    = 1 " Fold all trees but current file
  " Shifted version of NERDTree toggle
  map <Leader>< :TlistToggle<CR>

  let tlist_vimwiki_settings = 'vimwiki;h:Headers'
  let tlist_objc_settings    = 'objc;i:interface;c:class;m:method;p:property'

  " NERDTree
  let NERDTreeWinPos          = 'right'
  let NERDTreeShowBookmarks   = 1
  let NERDTreeQuitOnOpen      = 1      " hide after opening a file
  let NERDTreeMapActivateNode = '<CR>' " step in with Enter in addition to 'o'
  let NERDTreeIgnore          = ['\.git','\.hg','\.svn','\.DS_Store']
  let NERDTreeHijackNetrw     = 0      " I like netrw when I `:e somedir`

  " NERDCommenter
  let NERDSpaceDelims         = 1      " use a space after comment chars
  let NERDDefaultAlign        = 'left'
  " Not cool when end-of-line comments break when uncommenting /* */ blocks:
  let NERDRemoveAltComs       = 0

  " TaskList
  let g:tlWindowPosition      = 1      " TaskList on bottom
  let g:tlTokenList           = ['FIXME', 'TODO', 'CHANGED', 'PONY']
  map <leader>T <Plug>TaskList

  " Open the YankRing window
  nnoremap <silent> <M-v> :YRShow<CR>
  let g:yankring_history_dir = '$HOME/.autosave'
  " Make sure YankRing plays nice with custom remapping.
  " See `:h yankring-custom-maps`
  function! YRRunAfterMaps()
    nnoremap <silent> Y   :<C-U>YRYankCount 'y$'<CR>
  endfunction

  let g:dbext_default_history_file = '$HOME/.autosave'

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

endif " has("autocmd")

" Plugin Mappings {{{2

" Ack Search
map <Leader>a :Ack<space>

" NERD tree - double-leader
map <Leader><Leader> :NERDTreeToggle<cr>

" Command-T's <Leader>t default is good for files, but I use <Leader>b already
map <Leader>B :CommandTBuffer<CR>

" Ready for tab-completion of named Tabular patterns
" Choosing 'gq' since it's similar function to the format command
map <Leader>gq :Tabularize<space>

if has('python')
  " UltiSnips
  let g:UltiSnipsExpandTrigger       = "<tab>"
  let g:UltiSnipsJumpForwardTrigger  = "<tab>"
  let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"
  nmap <Leader>rs :py UltiSnips_Manager.reset()<CR>

  " Gundo
  nnoremap <F7> :GundoToggle<CR>
  let g:gundo_preview_bottom = 1     " force wide window across bottom

  " Sparkup
  " Way to default to a mapping that conflicts with scrolling, guy (<C-e>)...
  let g:sparkupExecuteMapping = "<C-s>"

  " ConqueTerm - namespace of 'q' kinda makes sense to me
  " mnemonic: terminal - don't like 'shell' because 'qs' is slow to type
  nmap <Leader>qt :ConqueTermSplit bash<CR>
  " mnemonic: command
  nmap <Leader>qc :ConqueTermSplit<space>
  let g:ConqueTerm_CloseOnEnd = 1
  " Exec current file in new split term. By default this conflicts with my
  " <F11> mapping for toggling hlsearch. <F10> execs current file in last-used
  " existing term, so let's use the shifted version of that
  let g:ConqueTerm_ExecFileKey = '<S-F10>'

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

noremap <C-g>v :Gitv --all<CR>
noremap <C-g>V :Gitv! --all<CR>

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

if has('python')
  " Just starting to play with possibilities here :-)
  function! MyConqueStartup(term)
    let syntax_associations = { 'mongo': 'javascript' }

    if has_key(syntax_associations, a:term.program_name)
      execute 'setlocal syntax=' . syntax_associations[a:term.program_name]
    endif
  endfunction

  call conque_term#register_function('after_startup', 'MyConqueStartup')
endif

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

" Plugin distributed with Vim to bring forward existing session w/ open file
runtime! macros/editexisting.vim

" vim:foldmethod=marker commentstring="%s

