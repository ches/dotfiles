call neobundle#begin(expand('~/.vim/bundle'))

NeoBundleFetch 'Shougo/neobundle.vim'

" Plugin Bundles {{{1

" From GitHub
if has('mac')
  NeoBundle 'rizzatti/funcoo.vim'
  NeoBundle 'rizzatti/dash.vim'
endif

NeoBundle 'alfredodeza/chapa.vim'
NeoBundle 'alfredodeza/konira.vim'
NeoBundle 'AndrewRadev/vim-eco'
NeoBundle 'AndrewRadev/splitjoin.vim'
NeoBundle 'bling/vim-airline'
NeoBundle 'chase/vim-ansible-yaml'
NeoBundle 'claco/jasmine.vim'
NeoBundle 'derekwyatt/vim-sbt'
NeoBundle 'derekwyatt/vim-scala'
NeoBundle 'eiginn/netrw'
NeoBundle 'embear/vim-localvimrc'
NeoBundle 'ecomba/vim-ruby-refactoring'
NeoBundle 'elixir-lang/vim-elixir'
NeoBundle 'fatih/vim-go'
NeoBundle 'fs111/pydoc.vim'
NeoBundle 'Glench/Vim-Jinja2-Syntax'
NeoBundle 'godlygeek/tabular'
NeoBundle 'gregsexton/gitv'
NeoBundle 'hdima/python-syntax'
NeoBundle 'honza/dockerfile.vim'
NeoBundle 'honza/vim-snippets'
NeoBundle 'ivanov/vim-ipython'
NeoBundle 'jgdavey/tslime.vim'
NeoBundle 'jgdavey/vim-turbux'
NeoBundle 'jiangmiao/auto-pairs'
NeoBundle 'jimenezrick/vimerl'
NeoBundle 'jmcantrell/vim-virtualenv'
NeoBundle 'kana/vim-textobj-user'
NeoBundle 'kchmck/vim-coffee-script'
NeoBundle 'kien/ctrlp.vim'
NeoBundle 'klen/rope-vim'
NeoBundle 'kshenoy/vim-signature'
NeoBundle 'LStinson/TagmaTasks'
NeoBundle 'ludovicchabant/vim-lawrencium'
NeoBundle 'majutsushi/tagbar'
NeoBundle 'mattn/emmet-vim'
NeoBundle 'mattn/gist-vim'
NeoBundle 'mattn/webapi-vim'
NeoBundle 'mhinz/vim-signify'
NeoBundle 'mileszs/ack.vim'
NeoBundle 'msanders/cocoa.vim'
NeoBundle 'nelstrom/vim-textobj-rubyblock'
NeoBundle 'nvie/vim-pep8'
NeoBundle 'rygwdn/rope-omni'
NeoBundle 'scrooloose/nerdtree'
NeoBundle 'scrooloose/syntastic'
NeoBundle 'SirVer/ultisnips'
" Look at mbbill/undotree as a pure VimL alternative:
NeoBundle 'sjl/gundo.vim'
NeoBundle 'skwp/greplace.vim'
NeoBundle 'swaroopch/vim-markdown-preview'
" switch from scrooloose fork for NERDDefaultAlign:
NeoBundle 'ervandew/nerdcommenter'
NeoBundle 'pangloss/vim-javascript'
NeoBundle 'rodjek/vim-puppet'
NeoBundle 'timcharper/textile.vim'
NeoBundle 'tpope/vim-abolish'
NeoBundle 'tpope/vim-bundler'
NeoBundle 'tpope/vim-characterize'
NeoBundle 'tpope/vim-cucumber'
NeoBundle 'tpope/vim-dispatch'
NeoBundle 'tpope/vim-endwise'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'tpope/vim-git'
NeoBundle 'tpope/vim-haml'
NeoBundle 'tpope/vim-markdown'
NeoBundle 'tpope/vim-ragtag'
NeoBundle 'tpope/vim-rake'
NeoBundle 'tpope/vim-rails', { 'stay_same' : 1 }
NeoBundle 'tpope/vim-repeat'
NeoBundle 'tpope/vim-rvm'
NeoBundle 'tpope/vim-surround'
NeoBundle 'tpope/vim-unimpaired'
NeoBundle 'wting/rust.vim'
NeoBundle 'vim-ruby/vim-ruby'
NeoBundle 'vimwiki/vimwiki'
NeoBundle 'xolox/vim-misc'
NeoBundle 'xolox/vim-session'

" From vim-scripts GitHub account (vimscripts.org mirrors)
" TODO: change all to vim-scripts GitHub account
" TODO: replace a.vim w/ derekwyatt/vim-fswitch or tpope/projectionist.vim
NeoBundle 'a.vim'
NeoBundle 'dbext.vim'
NeoBundle 'jQuery'
NeoBundle 'LustyJuggler'
NeoBundle 'matchit.zip'
NeoBundle 'TailMinusF'
NeoBundle 'word_complete.vim'
NeoBundle 'YankRing.vim'

" Custom git repo locations
NeoBundle 'git://repo.or.cz/vcscommand'

" Color schemes. Slightly nuts to manage with Vundle I guess, but easy updates.
NeoBundle 'altercation/vim-colors-solarized'
NeoBundle 'chriskempson/base16-vim'
NeoBundle 'godlygeek/csapprox', { 'terminal' : 1 }
NeoBundle 'gregsexton/Atom'
NeoBundle 'nanotech/jellybeans.vim'
NeoBundle 'nelstrom/vim-blackboard'
NeoBundle 'tomasr/molokai'
NeoBundle 'tpope/vim-vividchalk'

call neobundle#end()

" vim:foldmethod=marker commentstring="%s

