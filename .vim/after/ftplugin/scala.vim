" The ensime build tool plugins like sbt-ensime basically take care of setting
" up the prequisites for ensime-vim (installing the ENSIME server, etc.). So
" the build plugin should be the only setup needed.

if neobundle#is_installed('ensime-vim')
  " This should be automatic but it seems like Eclim is getting in the way,
  " should probably get rid of that...
  setlocal omnifunc=EnCompleteFunc

  nnoremap <buffer><silent> <LocalLeader>e  :EnClasspath<CR>
  nnoremap <buffer><silent> <LocalLeader>t  :EnType<CR>
  nnoremap <buffer>         <LocalLeader>T  :EnToggleFullType<CR>
  nnoremap <buffer><silent> <LocalLeader>i  :EnInspectType<CR>
  nnoremap <buffer><silent> <LocalLeader>I  :EnSuggestImport<CR>

  " Go to declaration/definition. These follow from Vim's native tag mappings.
  nnoremap <buffer> <LocalLeader><C-]> :EnDeclaration<CR>
  nnoremap <buffer> <silent> gd :EnDeclaration<CR>
  nnoremap <buffer> <silent> <LocalLeader><C-w>]      :EnDeclarationSplit<CR>
  nnoremap <buffer> <silent> <LocalLeader><C-w><C-]>  :EnDeclarationSplit<CR>
  " TODO: implement a preview window version, <C-w>}

  " Rename identifier under cursor
  nnoremap <buffer> <silent> <LocalLeader>r :EnRename<CR>

  " Not sure how this is supposed to work yet... would like something better
  " than BROWSER variable for :EnDocBrowse
  nnoremap <buffer> <silent> gK :EnDocUri<CR>
endif
