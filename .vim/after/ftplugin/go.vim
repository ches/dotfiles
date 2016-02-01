" Kick things off with :GoInstallBinaries to get all the tools installed to
" support vim-go's features. Set up your GOPATH first, or specify
" g:go_bin_path.
"
" TODO: :GoBuild!, :GoRun!, and :GoTest stuff to Dispatch
" TODO: Mappings for the doc lookup <Plug>s. Vim convention, Python?
" TODO: <Plug>(go-def), i.e. :GoDef, `gd`, etc. should use Vim's tag stack
" TODO: Include the Tagbar type def in vim-go

" vim-go doesn't add typical values to formatoptions, it only subtracts t
" Why isn't my vimrc autocmd overriding vim-go? NeoBundleLazy?
setlocal formatoptions+=roj

" Mappings
"
" Run `go build` for the project - keep or config for Dispatch?
nmap <buffer> <silent> <LocalLeader>b <Plug>(go-build)

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

nmap <buffer> <silent> gK <Plug>(go-doc-vertical)
nmap <buffer> <silent> <Leader>K <Plug>(go-doc-browser)

" Quick vet. Use `:SyntasticCheck govet` instead?
nmap <buffer> <silent> <LocalLeader>v <Plug>(go-vet)
