" For a full experience, do the following and a have a coffee while everything
" compiles:
"
"   $ cabal install --user ghc-mod codex hasktags pointfree
"
" Make sure ~/.cabal/bin is on PATH.
" See my ~/.codex for customized tagsCmd.

" Prime is often used in Haskell; dot is usually a module separator which we
" want tag lookups to include. TODO: qualified import names, hothasktags?
setlocal iskeyword+=.,'

" Check out lushtags, but it needs a pull request for cabal stuff merged:
"   https://github.com/bitc/lushtags
if executable('hasktags')
  " TODO: consider git ctags hook .git/tags
  setlocal tags=tags;/,codex.tags;/
  nmap <buffer> <LocalLeader>gt :!codex update<CR>
endif

" Take a look at stylish-haskell stuff in haskell-vim-now too.
if executable('pointfree')
  setlocal formatprg=xargs\ -0\ pointfree
endif

if neobundle#is_installed('ghcmod-vim')
  nmap <buffer> <LocalLeader>t  :GhcModType<CR>
  nmap <buffer> <LocalLeader>T  :GhcModTypeInsert<CR>
  nmap <buffer> <LocalLeader>c  :GhcModTypeClear<CR>
  nmap <buffer> <LocalLeader>i  :GhcModInfo<CR>
  nmap <buffer> <LocalLeader>I  :GhcModInfoPreview<CR>
  nmap <buffer> <LocalLeader>es :GhcModExpand<CR>
endif

" neco-ghc integrates with YouCompleteMe through its omnifunc support.
"
" Automatic display of completions doesn't seem to work—works on '.' and
" manual Ctrl-Space invocation of YCM. See:
"   https://github.com/eagletmt/neco-ghc/issues/17
if neobundle#is_installed('neco-ghc')
  setlocal omnifunc=necoghc#omnifunc
endif

