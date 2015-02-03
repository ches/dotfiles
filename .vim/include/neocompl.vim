" Configures either neocomplete or neocomplcache, depending on whether Lua is
" available.
"
" OS X:
"   brew install macvim --with-lua --with-luajit --override-system-vim
"
" Unfortunately UltiSnips' integration only works with neocomplete, a separate
" plugin is used for neocomplcache. Add this in ~/.vim/include/bundles.vim and
" remove YouCompleteMe:
"
" if has('lua')
"   NeoBundle 'Shougo/neocomplete'
" else
"   NeoBundle 'Shougo/neocomplcache'
"   NeoBundle 'JazzCore/neocomplcache-ultisnips'
" endif
"
" Might need to use NeoBundle's dependency support to do that in a way that
" doesn't cause NeoBundle.lock to change when running a Vim without Lua.
if has('lua')
  let g:neocomplete#enable_at_startup = 1
  let g:neocomplete#enable_smart_case = 1

  " Because rails.vim sets a completefunc.
  " TODO: autocmd to turn off and on when necessary
  let g:neocomplete#force_overwrite_completefunc = 1

  " Eclim
  " Note that YouCompleteMe wraps Eclim and other semantic completion engines
  " with a standard control interface. But YCM was crashy for me.
  let g:EclimCompletionMethod = 'omnifunc'

  if !exists('g:neocomplete#force_omni_input_patterns')
    let g:neocomplete#force_omni_input_patterns = {}
  endif
  " TODO: make a function to toggle this, it disables other sources like ctags
  " And see the help about using Erlang omnicomplete too. Sigh.
  " let g:neocomplete#force_omni_input_patterns.java = '\k\.\k*'
  " let g:neocomplete#force_omni_input_patterns.scala = '\k\.\k*'

  let g:neocomplete#force_omni_input_patterns.ruby = '[^. *\t]\.\w*\|\h\w*::'
  " https://github.com/vim-erlang/vim-erlang-omnicomplete
  " let g:neocomplete#force_omni_input_patterns.erlang =
  " \ '\<[[:digit:][:alnum:]_-]\+:[[:digit:][:alnum:]_-]*'

  " Recommended key-mappings.
  " <CR>: close popup and save indent.
  inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
  " <TAB>: completion. Conflict with UltiSnips?
  inoremap <expr><TAB>   pumvisible() ? "\<C-n>" : "\<TAB>"
  " <C-h>, <BS>: close popup and delete backword char.
  inoremap <expr><C-h>   neocomplete#smart_close_popup() . "\<C-h>"
  inoremap <expr><BS>    neocomplete#smart_close_popup() . "\<C-h>"
  inoremap <expr><C-y>   neocomplete#close_popup()
  inoremap <expr><C-e>   neocomplete#cancel_popup()
  " Close popup by <Space>.
  inoremap <expr><Space> pumvisible() ? neocomplete#close_popup() : "\<Space>"

  function! s:my_cr_function()
    return neocomplete#close_popup() . "\<CR>"
    " To not insert <CR>:
    " return pumvisible() ? neocomplete#close_popup() : "\<CR>"
  endfunction
else
  let g:neocomplcache_enable_at_startup = 1
  let g:neocomplcache_enable_smart_case = 1
  let g:neocomplcache_force_overwrite_completefunc = 1

  inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
  inoremap <expr><TAB>   pumvisible() ? "\<C-n>" : "\<TAB>"
  inoremap <expr><C-h>   neocomplcache#smart_close_popup() . "\<C-h>"
  inoremap <expr><BS>    neocomplcache#smart_close_popup() . "\<C-h>"
  inoremap <expr><C-y>   neocomplcache#close_popup()
  inoremap <expr><C-e>   neocomplcache#cancel_popup()
  inoremap <expr><Space> pumvisible() ? neocomplcache#close_popup() : "\<Space>"

  function! s:my_cr_function()
    return neocomplcache#smart_close_popup() . "\<CR>"
  endfunction
endif

