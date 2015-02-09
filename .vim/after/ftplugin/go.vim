" Kick things off with :GoInstallBinaries to get all the tools installed to
" support vim-go's features. Set up your GOPATH first, or specify
" g:go_bin_path.
"
" TODO: :GoBuild!, :GoRun!, and :GoTest stuff to Dispatch
" TODO: Mappings for the doc lookup <Plug>s. Vim convention, Python?

" Mappings
"
" Show type information for word under cursor
nnoremap <buffer> <silent> <LocalLeader>t <Plug>(go-info)

" Show interfaces that type under the cursor implements
nnoremap <buffer> <silent> <LocalLeader>i <Plug>(go-implements)

" Rename identifier under cursor
nnoremap <buffer> <silent> <LocalLeader>r <Plug>(go-rename)

" Go to declaration/definition. These follow from Vim's native tag mappings.
nnoremap <buffer> <silent> <LocalLeader><C-]>       <Plug>(go-def)
nnoremap <buffer> <silent> <LocalLeader><C-w>]      <Plug>(go-def-split)
nnoremap <buffer> <silent> <LocalLeader><C-w><C-]>  <Plug>(go-def-split)
" TODO: implement a preview window version, <C-w>}

" No Vim equivalents of these
nnoremap <buffer> <silent> <LocalLeader><C-v>]      <Plug>(go-def-vertical)
nnoremap <buffer> <silent> <LocalLeader><C-t>]      <Plug>(go-def-tab)

