" TODO: hook up :RustRun to Dispatch?

" Autocomplete and semantic go-to-definition: https://github.com/racer-rust/vim-racer
if neobundle#is_installed('vim-racer')
  " Go to definition that is likely smarter than ctags
  " TODO: should use Vim's tag stack
  nnoremap <buffer> <LocalLeader><C-]>      :call RacerGoToDefinition()<CR>
  nnoremap <buffer> <LocalLeader><C-w>]     :split<CR> :call RacerGoToDefinition()<CR>
  nnoremap <buffer> <LocalLeader><C-w><C-]> :split<CR> :call RacerGoToDefinition()<CR>
  " TODO: implement a preview window version, <C-w>}

  " No Vim equivalents of these
  nnoremap <buffer> <LocalLeader><C-v>] :vsplit<CR> :call RacerGoToDefinition()<CR>
  nnoremap <buffer> <LocalLeader><C-t>] :tab split<CR> :call RacerGoToDefinition()<CR>
endif
