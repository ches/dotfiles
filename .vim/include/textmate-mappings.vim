" This is my config for bringing over TextMate mappings that have become
" engrained. There are probably many like it, but this one is mine.
"
" 'D' is the Command key
"
" TODO: MacVim requires case sensitivity instead of explicit Shift,
" i.e. <D-R> instead of <S-D-r>. Anything we can do about <S-D-CR>?

" Toggle line wrapping
" TextMate uses Opt-Cmd-w but that closes all windows in MacVim...
map <silent> <C-M-w> :set invwrap<CR>
imap <silent> <C-M-w> <C-o>:set invwrap<CR>
vmap <silent> <C-M-w> :<C-u>:set invwrap<CR>gv

" Change double-quoting to single; requires surround.vim
" In reality this should cycle through quoting styles including language-
" specific styles like %Q{} in Ruby.
" TODO: <C-S-'> -- Specky plugin mimics TextMate here, but buggily
map <silent> <M-'> cs"'

if has('autocmd')
  augroup TextmateStuff
    autocmd!
    " insert hashrocket, =>
    " TODO: make it not greedy for dot-repeating?
    autocmd FileType ruby imap <C-l> <Space>=><Space>
    autocmd FileType coffee imap <C-l> <Space>->

    if has('mac') && has('gui_running')
      " Run tests/specs in Ruby/Rails apps
      autocmd User Rails,Rake nmap <D-r> :Rake<CR>
      autocmd User Rails,Rake nmap <D-R> :.Rake<CR>

      " 'Run' some filetypes the way TM would
      autocmd FileType coffee nmap <D-r> :CoffeeRun<CR>
      " The 'build' mapping in TM does compile-and-display
      autocmd FileType coffee nmap <D-b> :CoffeeCompile<CR>
    else
      " Left Option as Esc in iTerm2
      autocmd User Rails,Rake nmap <Esc>r :Rake<CR>
      autocmd User Rails,Rake nmap <Esc>R :.Rake<CR>
      autocmd FileType coffee nmap <Esc>r :CoffeeRun<CR>
      autocmd FileType coffee nmap <Esc>b :CoffeeCompile<CR>
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
  nmap <D-CR> o
  nmap <S-D-CR> O
  imap <D-CR> <Esc>o
  imap <S-D-CR> <Esc>A;<CR>

  " Cmd-bracket Indenting
  nmap <D-[> <<
  vmap <D-[> <
  imap <D-[> <C-d>
  nmap <D-]> >>
  vmap <D-]> >
  imap <D-]> <C-t>
  nmap <M-D-[> ==
  vmap <M-D-[> =
  imap <M-D-[> <C-f>

  " Toggle line numbers with Cmd-Opt-l
  nmap <silent> <D-M-l> :set invnumber<CR>
  imap <silent> <D-M-l> <C-o>:set invnumber<CR>
  vmap <silent> <D-M-l> :<C-u>:set invnumber<CR>gv

  " Toggle showing invisibles
  nmap <silent> <D-M-i> :set invlist<CR>
  imap <silent> <D-M-i> <C-o>:set invlist<CR>
  vmap <silent> <D-M-i> :<C-u>:set invlist<CR>gv

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
  nmap <Esc><CR> o
  nmap <Esc><S-CR> O
  imap <Esc><CR> <Esc>o
  imap <Esc><S-CR> <Esc>A;<CR>

  " Meta-bracket Indenting Using Ctrl to parallel Cmd from other TM commands
  " conflicts with Ctrl-[ as an Escape alternative, and Ctrl-] as tag jumping
  "
  " NOTE: something about my old visual mode mappings using <M-]>, etc. breaks
  " UltiSnips snippet expansion with Tab when running vim in console -- weird.
  nmap <Esc>[ <<
  vmap <Esc>[ <
  imap <Esc>[ <C-d>
  nmap <Esc>] >>
  vmap <Esc>] >
  imap <Esc>] <C-t>
  " nmap <M-C-[> ==
  " vmap <M-C-[> =
  " imap <M-C-[> <C-f>

  " Toggle line numbers with Shift-Alt-l
  nmap <silent> <Esc><S-l> :set invnumber<CR>
  imap <silent> <Esc><S-l> <C-o>:set invnumber<CR>
  vmap <silent> <Esc><S-l> :<C-u>:set invnumber<CR>gv

  " Toggle showing invisibles
  map <silent> <Esc><S-i> :set invlist<CR>
  imap <silent> <Esc><S-i> <C-o>:set invlist<CR>
  vmap <silent> <Esc><S-i> :<C-u>:set invlist<CR>gv

  " Toggle line wrapping
  map <silent> <Esc><S-w> :set invwrap<CR>
  imap <silent> <Esc><S-w> <C-o>:set invwrap<CR>
  vmap <silent> <Esc><S-w> :<C-u>:set invwrap<CR>gv
endif

