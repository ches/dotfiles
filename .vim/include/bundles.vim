call neobundle#begin(expand('~/.vim/bundle'))

NeoBundleFetch 'Shougo/neobundle.vim'

" Plugin Bundles {{{1

" System Dependent {{{2
NeoBundle 'rizzatti/dash.vim', { 'disabled' : !has('mac') }

" TODO: come up with a way to make this lazy
NeoBundle 'ludovicchabant/vim-lawrencium', {
\ 'augroup' : 'lawrencium_detect',
\ 'disabled' : !executable('hg')
\ }

if executable('opam') && executable('ocamlmerlin') && has('python')
  let g:opamshare = substitute(system('opam config var share'), '\n$', '', '''')

  NeoBundleLazy g:opamshare . '/merlin/vim', {
  \ 'name' : 'merlin',
  \ 'autoload' : { 'filetypes' : 'ocaml' }
  \ }
endif

" Always-on {{{2

NeoBundle 'Shougo/vimproc.vim', {
\ 'build' : {
\     'mac' : 'make -f make_mac.mak',
\     'linux' : 'make',
\    },
\ }

NeoBundle 'AndrewRadev/splitjoin.vim'
NeoBundle 'bling/vim-airline', { 'augroup' : 'airline' }
NeoBundle 'ciaranm/securemodelines'
NeoBundle 'ctrlpvim/ctrlp.vim'
NeoBundle 'direnv/direnv.vim'
NeoBundle 'eiginn/netrw', { 'augroup' : 'FileExplorer' }
NeoBundle 'embear/vim-localvimrc'
NeoBundle 'fisadev/vim-ctrlp-cmdpalette'
" NeoBundle 'gcmt/wildfire.vim'
NeoBundle 'glts/vim-textobj-comment'
NeoBundle 'godlygeek/tabular'
NeoBundle 'honza/vim-snippets'
NeoBundle 'jgdavey/tslime.vim'
NeoBundle 'jgdavey/vim-turbux'
NeoBundle 'jiangmiao/auto-pairs'
NeoBundle 'kana/vim-textobj-indent', { 'depends' : 'kana/vim-textobj-user' }
NeoBundle 'kana/vim-textobj-user'
NeoBundle 'kshenoy/vim-signature'
NeoBundle 'LStinson/TagmaTasks'
NeoBundle 'mhinz/vim-signify'
NeoBundle 'mileszs/ack.vim'
NeoBundle 'rking/ag.vim'
NeoBundle 'scrooloose/syntastic'
NeoBundle 'SirVer/ultisnips'
NeoBundle 'skwp/greplace.vim'
" switch from scrooloose fork for NERDDefaultAlign:
NeoBundle 'ervandew/nerdcommenter'
NeoBundle 'tpope/vim-abolish'
NeoBundle 'tpope/vim-characterize'
NeoBundle 'tpope/vim-dispatch'
NeoBundle 'tpope/vim-endwise'
NeoBundle 'tpope/vim-fugitive', { 'augroup' : 'fugitive' }
NeoBundle 'tpope/vim-git'
NeoBundle 'tpope/vim-markdown'
NeoBundle 'tpope/vim-projectionist'
NeoBundle 'tpope/vim-ragtag' " html, xml, haml, erb, php, jinja, django, ?
NeoBundle 'tpope/vim-repeat'
NeoBundle 'tpope/vim-surround'
NeoBundle 'tpope/vim-unimpaired'

" All the huge third-party submodule deps are annoying, but I've tried
" neocomplete and it's just unusably slow without tons of tuning :-/
NeoBundle 'Valloric/YouCompleteMe', {
\ 'augroup' : 'youcompletemeStart',
\ 'build_commands' : 'cmake',
\ 'build' : {
\    'mac'   : './install.py',
\    'linux' : './install.py'
\  }
\ }
NeoBundle 'vimwiki/vimwiki', 'dev'
NeoBundle 'xolox/vim-session', {
\ 'depends' : 'xolox/vim-misc',
\ 'augroup' : 'PluginSession'
\ }


" On-demand {{{2
" --------------

" Global - Oft-used, but heavy {{{3
" ---------------------------------
NeoBundleLazy 'scrooloose/nerdtree', {
\ 'autoload' : { 'commands' : ['NERDTree', 'NERDTreeToggle', 'NERDTreeFind'] },
\ 'explorer' : 1
\ }
NeoBundleLazy 'majutsushi/tagbar', {
\ 'autoload' : { 'commands' : 'TagbarToggle', 'functions' : 'tagbar#currenttag' },
\ 'augroup' : 'TagbarAutoCmds'
\ }
NeoBundleLazy 'sjbach/lusty', {
\ 'autoload' : { 'commands' : 'LustyJuggler' }
\ }
" Look at mbbill/undotree as a pure VimL alternative:
NeoBundleLazy 'sjl/gundo.vim', {
\ 'autoload' : { 'commands' : 'GundoToggle' }
\ }
NeoBundleLazy 'gregsexton/gitv', {
\ 'autoload' : { 'commands' : 'Gitv' },
\ 'depends'  : 'tpope/vim-fugitive'
\ }
NeoBundleLazy 'mattn/gist-vim', {
\ 'autoload' : { 'commands' : 'Gist' },
\ 'depends'  : 'mattn/webapi-vim'
\ }

" Haskell {{{3
" ------------
" Keep an eye on https://github.com/dag/vim2hs/issues/45
" Alternative indent/syntax: https://github.com/raichoo/haskell-vim
NeoBundleLazy 'dag/vim2hs', {
\ 'autoload' : { 'filetypes' : ['haskell', 'cabal'] },
\ }
" $ cabal install ghc-mod
NeoBundleLazy 'eagletmt/ghcmod-vim', {
\ 'autoload' : { 'filetypes' : 'haskell' },
\ 'depends'  : 'Shougo/vimproc.vim',
\ 'external_commands' : 'ghc-mod'
\ }
" Uses either YouCompleteMe or neocomplete/neocomplcache
NeoBundleLazy 'eagletmt/neco-ghc', {
\ 'autoload' : { 'filetypes' : 'haskell' },
\ 'depends'  : 'Valloric/YouCompleteMe',
\ 'external_commands' : 'ghc-mod'
\ }

" PHP {{{2
NeoBundle 'mkusher/padawan.vim', {
\ 'autoload' : { 'filetypes' : 'php' }
\ }

" Python {{{2
NeoBundleLazy 'alfredodeza/chapa.vim', {
\ 'autoload' : { 'filetypes' : ['python', 'javascript'] }
\ }
NeoBundleLazy 'alfredodeza/konira.vim', {
\ 'autoload' : { 'filetypes' : 'python' }
\ }
NeoBundleLazy 'davidhalter/jedi-vim', {
\ 'autoload' : { 'filetypes' : 'python' }
\ }
NeoBundleLazy 'Glench/Vim-Jinja2-Syntax', {
\ 'autoload' : { 'filename_patterns' : ['\.jinja2', '\.jinja$', '\.html$', '\.htm$'] }
\ }
NeoBundleLazy 'hdima/python-syntax', {
\ 'autoload' : { 'filetypes' : 'python' }
\ }
NeoBundleLazy 'ivanov/vim-ipython', {
\ 'autoload' : { 'filetypes' : 'python' }
\ }
NeoBundleLazy 'lambdalisue/vim-pyenv', {
\ 'autoload' : { 'filetypes' : ['python', 'python3'] },
\ 'depends' : 'davidhalter/jedi-vim'
\ }

" Ruby {{{3
NeoBundleLazy 'ecomba/vim-ruby-refactoring', {
\ 'autoload' : { 'filetypes' : 'ruby' }
\ }
NeoBundleLazy 'nelstrom/vim-textobj-rubyblock', {
\ 'autoload' : { 'filetypes' : 'ruby' },
\ 'depends' : 'kana/vim-textobj-user'
\ }
NeoBundleLazy 'tpope/vim-bundler', {
\ 'autoload' : { 'filetypes' : 'ruby' },
\ 'augroup' : 'bundler'
\ }
NeoBundleLazy 'tpope/vim-rake', {
\ 'autoload' : { 'filetypes' : 'ruby' },
\ 'augroup' : 'rake'
\ }
" TODO: needs a railsPluginAbolish augroup too
NeoBundleLazy 'tpope/vim-rails', {
\ 'autoload' : { 'filetypes' : 'ruby' },
\ 'augroup' : 'railsPluginDetect'
\ }
NeoBundleLazy 'tpope/vim-rvm', {
\ 'autoload' : { 'filetypes' : 'ruby' }
\ }
NeoBundleLazy 'vim-ruby/vim-ruby', {
\ 'autoload' : { 'filetypes' : 'ruby' }
\ }
NeoBundleLazy 'vim-scripts/Specky', {
\ 'autoload' : { 'filename_patterns' : '_spec\.rb$' }
\ }

" The Rest {{{3
NeoBundleLazy 'AndrewRadev/vim-eco', {
\ 'autoload' : { 'filename_patterns' : '\.eco$' }
\ }
NeoBundleLazy 'chase/vim-ansible-yaml', {
\ 'autoload' : { 'filetypes' : 'yaml' }
\ }
NeoBundleLazy 'claco/jasmine.vim', {
\ 'autoload' : { 'filetypes' : ['javascript', 'coffee'] }
\ }
NeoBundleLazy 'derekwyatt/vim-sbt', {
\ 'autoload' : { 'filename_patterns' : '\.sbt$' }
\ }
NeoBundleLazy 'derekwyatt/vim-scala', {
\ 'autoload' : { 'filetypes' : ['help', 'scala'] }
\ }
NeoBundleLazy 'ensime/ensime-vim', {
\ 'autoload' : { 'filetypes' : 'scala' }
\ }
NeoBundleLazy 'elixir-lang/vim-elixir', {
\ 'autoload' : { 'filetypes' : 'elixir' }
\ }
NeoBundleLazy 'fatih/vim-go', {
\ 'autoload' : { 'filetypes' : 'go' }
\ }
NeoBundleLazy 'gevans/vim-ginkgo', {
\ 'autoload' : { 'filetypes' : ['go', 'ginkgo'] }
\ }
NeoBundleLazy 'honza/dockerfile.vim', {
\ 'autoload' : { 'filename_patterns' : 'Dockerfile$' }
\ }
NeoBundleLazy 'hspec/hspec.vim', {
\ 'autoload' : { 'filetypes' : 'haskell' }
\ }
NeoBundleLazy 'jimenezrick/vimerl', {
\ 'autoload' : { 'filetypes' : 'erlang' }
\ }
NeoBundleLazy 'junegunn/goyo.vim', {
\ 'autoload' : { 'commands' : 'Goyo' }
\ }
NeoBundleLazy 'kchmck/vim-coffee-script', {
\ 'autoload' : { 'filetypes' : ['coffee', 'markdown'] }
\ }
NeoBundleLazy 'mattn/emmet-vim', {
\ 'autoload' : { 'filetypes' : ['html', 'css', 'haml', 'slim', 'sass', 'scss', 'less', 'htmldjango'] }
\ }
NeoBundleLazy 'msanders/cocoa.vim', {
\ 'autoload' : { 'filetypes' : 'objc' }
\ }
NeoBundleLazy 'swaroopch/vim-markdown-preview', {
\ 'autoload' : { 'filetypes' : 'markdown' },
\ 'external_commands' : 'multimarkdown'
\ }
NeoBundleLazy 'pangloss/vim-javascript', {
\ 'autoload' : { 'filename_patterns' : ['\.js$', '\.jsm', 'Jakefile$'] }
\ }
NeoBundleLazy 'rodjek/vim-puppet', {
\ 'autoload' : { 'filename_patterns' : '\.pp$' }
\ }
NeoBundleLazy 'timcharper/textile.vim', {
\ 'autoload' : { 'filename_patterns' : '\.textile$' }
\ }
NeoBundleLazy 'tpope/vim-cucumber', {
\ 'autoload' : { 'filetypes' : 'cucumber' }
\ }
NeoBundleLazy 'tpope/vim-haml', {
\ 'autoload' : { 'filetypes' : 'haml' }
\ }
NeoBundleLazy 'tpope/vim-scriptease', {
\ 'autoload' : {
\   'filetypes' : ['vim', 'help'],
\   'commands' : ['Disarm', 'PP', 'Scriptnames', 'Verbose',
\                 'Vedit', 'Vopen', 'Vsplit', 'Vvsplit']
\  }
\ }
NeoBundleLazy 'rust-lang/rust.vim', {
\ 'autoload' : { 'filetypes' : 'rust', 'filename_patterns' : '\.rs$' }
\ }
NeoBundleLazy 'racer-rust/vim-racer', {
\ 'autoload' : { 'filetypes' : 'rust', 'filename_patterns' : '\.rs$' }
\ }
NeoBundleLazy 'cespare/vim-toml', {
\ 'autoload' : { 'filename_patterns' : ['\.toml$', 'Cargo.lock'] }
\ }
" }}}


" From vim-scripts GitHub account (vimscripts.org mirrors)
" TODO: change all to vim-scripts GitHub account
NeoBundle 'dbext.vim'
NeoBundle 'jQuery'
NeoBundle 'TailMinusF'
NeoBundle 'word_complete.vim'
NeoBundle 'YankRing.vim', { 'augroup' : 'YankRing' }

" Custom git repo locations
NeoBundle 'git://repo.or.cz/vcscommand'

" Color schemes {{{2
" Slightly nuts to manage with Vundle I guess, but easy updates.
NeoBundle 'altercation/vim-colors-solarized'
NeoBundle 'chriskempson/base16-vim'
NeoBundle 'godlygeek/csapprox', { 'terminal' : 1 }
NeoBundle 'gregsexton/Atom'
NeoBundle 'nanotech/jellybeans.vim'
NeoBundle 'nelstrom/vim-blackboard'
NeoBundle 'tomasr/molokai'
NeoBundle 'tpope/vim-vividchalk'
" }}}

call neobundle#end()

" vim:foldmethod=marker commentstring="%s

