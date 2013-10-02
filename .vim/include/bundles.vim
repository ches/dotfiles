set runtimepath+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'

" Plugin Bundles {{{1

" From GitHub
if has('mac')
  Bundle 'rizzatti/funcoo.vim'
  Bundle 'rizzatti/dash.vim'
endif

Bundle 'alfredodeza/chapa.vim'
Bundle 'alfredodeza/konira.vim'
Bundle 'altercation/vim-colors-solarized'
Bundle 'AndrewRadev/vim-eco'
Bundle 'AndrewRadev/splitjoin.vim'
Bundle 'claco/jasmine.vim'
Bundle 'derekwyatt/vim-scala'
Bundle 'dhazel/conque-term'
Bundle 'ecomba/vim-ruby-refactoring'
Bundle 'ervandew/supertab'
Bundle 'fs111/pydoc.vim'
Bundle 'zakj/vim-showmarks'
Bundle 'godlygeek/tabular'
Bundle 'godlygeek/csapprox'
Bundle 'gregsexton/gitv'
" Bundle 'greyblake/vim-preview'  " Too many damned external dependencies
Bundle 'ivanov/vim-ipython'
Bundle 'jgdavey/tslime.vim'
Bundle 'jgdavey/vim-turbux'
Bundle 'jmcantrell/vim-virtualenv'
Bundle 'kana/vim-textobj-user'
Bundle 'kchmck/vim-coffee-script'
Bundle 'klen/rope-vim'
Bundle 'Lokaltog/powerline'
Bundle 'LStinson/TagmaTasks'
Bundle 'majutsushi/tagbar'
Bundle 'mattn/gist-vim'
Bundle 'mattn/webapi-vim'
Bundle 'mileszs/ack.vim'
Bundle 'msanders/cocoa.vim'
Bundle 'nanotech/jellybeans.vim'
Bundle 'nelstrom/vim-blackboard'
Bundle 'nelstrom/vim-textobj-rubyblock'
Bundle 'nvie/vim-pep8'
Bundle 'rygwdn/rope-omni'
Bundle 'scrooloose/nerdtree'
Bundle 'scrooloose/syntastic'
Bundle 'SirVer/ultisnips'
Bundle 'sjl/gundo.vim'
Bundle 'skwp/greplace.vim'
Bundle 'swaroopch/vim-markdown-preview'
" switch from scrooloose fork for NERDDefaultAlign:
Bundle 'ervandew/nerdcommenter'
Bundle 'pangloss/vim-javascript'
Bundle 'rodjek/vim-puppet'
Bundle 'timcharper/textile.vim'
Bundle 'tpope/vim-abolish'
Bundle 'tpope/vim-bundler'
Bundle 'tpope/vim-cucumber'
Bundle 'tpope/vim-endwise'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-git'
Bundle 'tpope/vim-haml'
Bundle 'tpope/vim-markdown'
Bundle 'tpope/vim-ragtag'
Bundle 'tpope/vim-rake'
Bundle 'tpope/vim-rails'
Bundle 'tpope/vim-repeat'
Bundle 'tpope/vim-rvm'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-unimpaired'
Bundle 'tpope/vim-vividchalk'
Bundle 'wincent/Command-T'
" Giving Syntastic a spin, would like to settle on it for consolidation but
" wish they'd fix things like:
"       https://github.com/scrooloose/syntastic/pull/46
" Bundle 'wookiehangover/jshint.vim'
Bundle 'vim-ruby/vim-ruby'
Bundle 'xolox/vim-misc'
Bundle 'xolox/vim-session'

" From vim-scripts GitHub account (vimscripts.org mirrors)
Bundle 'a.vim'
Bundle 'dbext.vim'
Bundle 'jQuery'
Bundle 'LustyJuggler'
Bundle 'matchit.zip'
Bundle 'TailMinusF'
Bundle 'taglist.vim'
Bundle 'vimwiki'
Bundle 'word_complete.vim'
Bundle 'YankRing.vim'

" Custom git repo locations
Bundle 'git://repo.or.cz/vcscommand'

" vim:foldmethod=marker commentstring="%s

