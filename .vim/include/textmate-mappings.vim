" This is my config for bringing over TextMate mappings that have become
" engrained. There are probably many like it, but this one is mine.
"
" 'D' is the Command key

" Toggle line wrapping
" TextMate uses Opt-Cmd-w but that closes all windows in MacVim...
map <silent> <C-M-w> :set invwrap<CR>
imap <silent> <C-M-w> <C-o>:set invwrap<CR>
vmap <silent> <C-M-w> :<C-u>:set invwrap<CR>gv

if has('mac')

  " Option key as meta
  set macmeta

  " Cmd-/ Comment Toggling
  nmap <D-/> <Plug>NERDCommenterToggle
  imap <D-/> <Esc><Plug>NERDCommenterToggle i
  vmap <D-/> <Plug>NERDCommenterToggle

  " New line conveniences
  " Shifted versions don't seem to work due to same keycodes...
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

  " Toggle showing invisibles
  map <silent> <D-M-i> :set invlist<CR>
  imap <silent> <D-M-i> <C-o>:set invlist<CR>
  vmap <silent> <D-M-i> :<C-u>:set invlist<CR>gv

else

  " Ctrl-/ Comment Toggling
  nmap <C-/> <Plug>NERDCommenterToggle
  imap <C-/> <Esc><Plug>NERDCommenterToggle i
  vmap <C-/> <Plug>NERDCommenterToggle

  " New line conveniences
  " Shifted versions don't seem to work due to same keycodes...
  nmap <C-CR> o
  nmap <S-C-CR> O
  imap <C-CR> <Esc>o
  imap <S-C-CR> <Esc>A;<CR>

  " Ctrl-bracket Indenting
  nmap <C-[> <<
  vmap <C-[> <
  imap <C-[> <C-d>
  nmap <C-]> >>
  vmap <C-]> >
  imap <C-]> <C-t>
  nmap <M-C-[> ==
  vmap <M-C-[> =
  imap <M-C-[> <C-f>

  " Toggle showing invisibles
  map <silent> <C-F11> :set invlist<CR>
  imap <silent> <C-F11> <C-o>:set invlist<CR>
  vmap <silent> <C-F11> :<C-u>:set invlist<CR>gv

endif

