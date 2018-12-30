if neobundle#is_installed('merlin')
  " let g:merlin_split_method = "vertical"

  nnoremap <buffer> <silent> K :MerlinDocument<CR>

  " Show type of expr under cursor -- overrides mapping from Vim's ocaml runtime that doesn't work well
  nnoremap <buffer> <LocalLeader>t :MerlinTypeOf<CR>

  " Go to definition that is likely smarter than ctags; Merlin binds this to gd already
  nnoremap <buffer> <LocalLeader><C-]> :MerlinLocate<CR>

  " Similar to CtrlPTag for global symbols
  nnoremap <buffer> <LocalLeader><C-f> :MerlinILocate<CR>
  " And this is much like CtrlPBufTag
  nnoremap <buffer> <LocalLeader>f :MerlinOutline<CR>

  " Yank last type shown with :TypeOf to "r register
  nnoremap <buffer> <LocalLeader>yt :MerlinYankLatestType<CR> :echo 'Type copied to "r register'<CR>

  " Toggle a buffer of type lookups, useful for declaring interfaces
  nnoremap <buffer> <LocalLeader>ht :MerlinToggleTypeHistory<CR>

  " Semantic identifier search
  nmap <buffer> <silent> <LocalLeader>*  <Plug>(MerlinSearchOccurrencesForward)
  nmap <buffer> <silent> <LocalLeader>#  <Plug>(MerlinSearchOccurrencesBackward)

  nmap <buffer> <silent> <LocalLeader>r  <Plug>(MerlinRename)
  nmap <buffer> <silent> <LocalLeader>R  <Plug>(MerlinRenameAppend)

  nnoremap <buffer> <silent> <LocalLeader>u :MerlinOccurrences<CR>
end

