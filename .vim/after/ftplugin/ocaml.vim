if neobundle#is_installed('merlin')
  " <LocalLeader>t is bound to :TypeOf by default (show type of expr under cursor)

  " Go to definition that is likely smarter than ctags
  nnoremap <buffer> <LocalLeader><C-]> :Locate<CR>

  " Yank last type shown with :TypeOf to "r register
  nnoremap <buffer> <LocalLeader>yt :YankLatestType<CR> :echo 'Type copied to "r register'<CR>

  " Toggle a buffer of type lookups, useful for declaring interfaces
  nnoremap <buffer> <LocalLeader>ht :ToggleTypeHistory<CR>

  " Semantic identifier search
  " To turn off automatic listing window (:Occurrences, seems broken?):
  " let g:merlin_display_occurrence_list = 0
  nnoremap <buffer> <silent> <LocalLeader>*  <Plug>(MerlinSearchOccurencesForward)
  nnoremap <buffer> <silent> <LocalLeader>#  <Plug>(MerlinSearchOccurencesBackward)

  nnoremap <buffer> <silent> <LocalLeader>r  <Plug>(MerlinRename)
  nnoremap <buffer> <silent> <LocalLeader>R  <Plug>(MerlinRenameAppend)
end

