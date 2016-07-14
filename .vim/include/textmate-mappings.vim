" This is my config for bringing over TextMate mappings that have become
" engrained. There are probably many like it, but this one is mine.
"
" 'D' is the Command key
"
" TODO: MacVim requires case sensitivity instead of explicit Shift,
" i.e. <D-R> instead of <S-D-r>. Anything we can do about <S-D-CR>?

scriptencoding utf-8

" Change double-quoting to single; requires surround.vim
" In reality this should cycle through quoting styles including language-
" specific styles like %Q{} in Ruby.
" TODO: <C-S-'> -- Specky plugin mimics TextMate here, but buggily
map <silent> <M-'> cs"'

if has('autocmd')
  augroup TextmateStuff
    autocmd!
    " Fat vs. skinny versions may seem inconsistent, but based on most used in the lang
    " TODO: make it not greedy for dot-repeating?
    " TODO: Meta versions don't work in terminal when mapping Option to Escape
    " in order to make many other mappings more functional. At least the
    " digraphs are intuitive to remember, e.g. <C-k>=> and <C-k>->
    autocmd FileType ruby    inoremap <buffer> <C-l> <Space>=><Space>
    autocmd FileType coffee  inoremap <buffer> <C-l> <Space>->
    autocmd FileType coffee  inoremap <buffer> <C-L> <Space>=>
    autocmd FileType cpp     inoremap <buffer> <C-l> ->
    autocmd FileType go      inoremap <buffer> <C-l> <-
    autocmd FileType haskell inoremap <buffer> <C-l> <Space>-><Space>
    autocmd FileType haskell inoremap <buffer> <C-L> <Space>=><Space>
    autocmd FileType haskell inoremap <buffer> <M-l> <Space><-<Space>
    autocmd FileType erlang  inoremap <buffer> <C-l> <Space>->
    autocmd FileType erlang  inoremap <buffer> <C-L> <Space>=>
    autocmd FileType php     inoremap <buffer> <C-l> ->
    autocmd FileType scala   inoremap <buffer> <C-l> <Space>=><Space>
    autocmd FileType scala   inoremap <buffer> <C-L> <Space>-><Space>
    autocmd FileType scala   inoremap <buffer> <M-l> <Space>⇒<Space>
    autocmd FileType scala   inoremap <buffer> <M-L> <Space>→<Space>

    " NOTE: The MacVim GUI may map Cmd-B to a menu item for :make. Might need
    " to override this for some cases below.
    " TODO: dispatch.vim?
    if has('mac') && has('gui_running')
      " Run tests/specs in Ruby/Rails apps
      autocmd User Rails,Rake nnoremap <buffer> <D-r> :Rake<CR>
      autocmd User Rails,Rake nnoremap <buffer> <D-R> :.Rake<CR>

      " 'Run' some filetypes the way TM would
      autocmd FileType coffee nnoremap <buffer> <D-r> :CoffeeRun<CR>
      " The 'build' mapping in TM does compile-and-display
      autocmd FileType coffee nnoremap <buffer> <D-b> :CoffeeCompile<CR>
    else
      " Left Option as Esc in iTerm2
      autocmd User Rails,Rake nnoremap <buffer> <Esc>r :Rake<CR>
      autocmd User Rails,Rake nnoremap <buffer> <Esc>R :.Rake<CR>
      autocmd FileType coffee nnoremap <buffer> <Esc>r :CoffeeRun<CR>
      autocmd FileType coffee nnoremap <buffer> <Esc>b :CoffeeCompile<CR>
    endif
  augroup END
endif

if has('mac') && has('gui_running')
  " Option key as meta
  set macmeta

  " Cmd-/ Comment Toggling
  nmap <D-/> <Plug>NERDCommenterToggle
  imap <D-/> <Esc><Plug>NERDCommenterToggle i
  vmap <D-/> <Plug>NERDCommenterToggle

  " New line conveniences
  " FIXME: shifted CR -- see above
  nnoremap <D-CR> o
  nnoremap <S-D-CR> O
  inoremap <D-CR> <Esc>o
  inoremap <S-D-CR> <Esc>A;<CR>

  " Cmd-bracket Indenting
  nnoremap <D-[> <<
  vnoremap <D-[> < gv
  inoremap <D-[> <C-d>
  nnoremap <D-]> >>
  vnoremap <D-]> > gv
  inoremap <D-]> <C-t>
  nnoremap <M-D-[> ==
  vnoremap <M-D-[> =
  inoremap <M-D-[> <C-f>
else
  " These don't match up as closely since modifier key handling portability on
  " the console is a slippery slope... see `:help :map-alt-keys` for instance.
  "
  " http://vim.1045645.n5.nabble.com/How-to-map-Ctrl-tp1192843p1192844.html
  "
  " For now the mappings below will assume a 'alt/option sends escape' config
  " option, which is convenient for OS X and probably other terminal
  " emulators. Frankly, it's all hardly worth the trouble and I've mostly
  " adopted the normal Vim mappings as habit now.

  " Opt-/ Comment Toggling
  nmap <Esc>/ <Plug>NERDCommenterToggle
  imap <Esc>/ <Esc><Plug>NERDCommenterToggle i
  vmap <Esc>/ <Plug>NERDCommenterToggle

  " New line conveniences
  " Shifted versions don't seem to work due to same keycodes...
  nnoremap <Esc><CR> o
  nnoremap <Esc><S-CR> O
  inoremap <Esc><CR> <Esc>o
  inoremap <Esc><S-CR> <Esc>A;<CR>

  " Meta-bracket Indenting Using Ctrl to parallel Cmd from other TM commands
  " conflicts with Ctrl-[ as an Escape alternative, and Ctrl-] as tag jumping
  "
  " NOTE: something about my old visual mode mappings using <M-]>, etc. breaks
  " UltiSnips snippet expansion with Tab when running vim in console -- weird.
  nnoremap <Esc>[ <<
  vnoremap <Esc>[ < gv
  inoremap <Esc>[ <C-d>
  nnoremap <Esc>] >>
  vnoremap <Esc>] > gv
  inoremap <Esc>] <C-t>
  " nnoremap <M-C-[> ==
  " vnoremap <M-C-[> =
  " inoremap <M-C-[> <C-f>
endif

