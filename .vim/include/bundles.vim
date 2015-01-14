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

" Always-on {{{2

NeoBundle 'AndrewRadev/splitjoin.vim'
NeoBundle 'bling/vim-airline', { 'augroup' : 'airline' }
NeoBundle 'eiginn/netrw', { 'augroup' : 'FileExplorer' }
NeoBundle 'embear/vim-localvimrc'
NeoBundle 'godlygeek/tabular'
NeoBundle 'honza/vim-snippets'
NeoBundle 'jgdavey/tslime.vim'
NeoBundle 'jgdavey/vim-turbux'
NeoBundle 'jiangmiao/auto-pairs'
NeoBundle 'kana/vim-textobj-user'  " Dependency of something?
NeoBundle 'kien/ctrlp.vim'
NeoBundle 'kshenoy/vim-signature'
NeoBundle 'LStinson/TagmaTasks'
NeoBundle 'mhinz/vim-signify'
NeoBundle 'mileszs/ack.vim'
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
NeoBundle 'tpope/vim-ragtag' " html, xml, haml, erb, php, jinja, django, ?
NeoBundle 'tpope/vim-repeat'
NeoBundle 'tpope/vim-surround'
NeoBundle 'tpope/vim-unimpaired'
NeoBundle 'Valloric/YouCompleteMe', {
\ 'augroup' : 'youcompletemeStart',
\ 'build_commands' : 'cmake',
\ 'build' : {
\    'mac'   : './install.sh',
\    'linux' : './install.sh'
\  }
\ }
NeoBundle 'vimwiki/vimwiki'
NeoBundle 'xolox/vim-session', {
\ 'depends' : 'xolox/vim-misc',
\ 'augroup' : 'PluginSession'
\ }


" On-demand {{{2
" --------------

" Global - Oft-used, but heavy {{{3
NeoBundleLazy 'scrooloose/nerdtree', {
\ 'autoload' : { 'commands' : ['NERDTree', 'NERDTreeToggle', 'NERDTreeFind'] },
\ 'explorer' : 1
\ }
NeoBundleLazy 'majutsushi/tagbar', {
\ 'autoload' : { 'commands' : 'TagbarToggle' },
\ 'augroup' : 'TagbarAutoCmds'
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

" Python {{{3
NeoBundleLazy 'alfredodeza/chapa.vim', {
\ 'autoload' : { 'filetypes' : 'python' }
\ }
NeoBundleLazy 'alfredodeza/konira.vim', {
\ 'autoload' : { 'filetypes' : 'python' }
\ }
NeoBundleLazy 'fs111/pydoc.vim', {
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
NeoBundleLazy 'jmcantrell/vim-virtualenv', {
\ 'autoload' : { 'filetypes' : 'python' }
\ }
NeoBundleLazy 'klen/rope-vim', {
\ 'autoload' : { 'filetypes' : 'python' }
\ }
NeoBundleLazy 'nvie/vim-pep8', {
\ 'autoload' : { 'filetypes' : 'python' }
\ }
NeoBundleLazy 'rygwdn/rope-omni', {
\ 'autoload' : { 'filetypes' : 'python' }
\ }

" Ruby {{{3
NeoBundleLazy 'ecomba/vim-ruby-refactoring', {
\ 'autoload' : { 'filetypes' : 'ruby' }
\ }
NeoBundleLazy 'nelstrom/vim-textobj-rubyblock', {
\ 'autoload' : { 'filetypes' : 'ruby' }
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
\ 'augroup' : 'railsPluginDetect',
\ 'stay_same' : 1
\ }
NeoBundleLazy 'tpope/vim-rvm', {
\ 'autoload' : { 'filetypes' : 'ruby' }
\ }
NeoBundleLazy 'vim-ruby/vim-ruby', {
\ 'autoload' : { 'filetypes' : 'ruby' }
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
\ 'autoload' : { 'filetypes' : 'scala' }
\ }
NeoBundleLazy 'elixir-lang/vim-elixir', {
\ 'autoload' : { 'filetypes' : 'elixir' }
\ }
NeoBundleLazy 'fatih/vim-go', {
\ 'autoload' : { 'filetypes' : 'go' }
\ }
NeoBundleLazy 'honza/dockerfile.vim', {
\ 'autoload' : { 'filename_patterns' : 'Dockerfile$' }
\ }
NeoBundleLazy 'jimenezrick/vimerl', {
\ 'autoload' : { 'filetypes' : 'erlang' }
\ }
NeoBundleLazy 'kchmck/vim-coffee-script', {
\ 'autoload' : { 'filename_patterns' : ['\.coffee$', 'Cakefile$'] }
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
NeoBundleLazy 'wting/rust.vim', {
\ 'autoload' : { 'filetypes' : 'rust' }
\ }


" From vim-scripts GitHub account (vimscripts.org mirrors)
" TODO: change all to vim-scripts GitHub account
" TODO: replace a.vim w/ derekwyatt/vim-fswitch or tpope/projectionist.vim
NeoBundle 'a.vim'
NeoBundle 'dbext.vim'
NeoBundle 'jQuery'
NeoBundle 'LustyJuggler'
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

