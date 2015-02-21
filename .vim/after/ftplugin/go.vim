" Kick things off with :GoInstallBinaries to get all the tools installed to
" support vim-go's features. Set up your GOPATH first, or specify
" g:go_bin_path.
"
" TODO: :GoBuild!, :GoRun!, and :GoTest stuff to Dispatch
" TODO: Mappings for the doc lookup <Plug>s. Vim convention, Python?
" TODO: <Plug>(go-def), i.e. :GoDef, `gd`, etc. should use Vim's tag stack

" Mappings
"
" Show type information for word under cursor
nmap <buffer> <silent> <LocalLeader>t <Plug>(go-info)

" Show interfaces that type under the cursor implements
nmap <buffer> <silent> <LocalLeader>i <Plug>(go-implements)

" Rename identifier under cursor
nmap <buffer> <silent> <LocalLeader>r <Plug>(go-rename)

" Go to declaration/definition. These follow from Vim's native tag mappings.
nmap <buffer> <silent> <LocalLeader><C-]>       <Plug>(go-def)
nmap <buffer> <silent> <LocalLeader><C-w>]      <Plug>(go-def-split)
nmap <buffer> <silent> <LocalLeader><C-w><C-]>  <Plug>(go-def-split)
" TODO: implement a preview window version, <C-w>}

" No Vim equivalents of these
nmap <buffer> <silent> <LocalLeader><C-v>]      <Plug>(go-def-vertical)
nmap <buffer> <silent> <LocalLeader><C-t>]      <Plug>(go-def-tab)

