" Make help navigation easier on the hands, Vimwiki-style.
nnoremap <buffer> <CR> <C-]>
nnoremap <buffer> <BS> <C-t>

" Intentionally recursive for below mappings
nmap <buffer> <Tab>   ]s
nmap <buffer> <S-Tab> [s

" Navigate to next/previous option or subject.
" http://vim.wikia.com/wiki/Learn_to_use_help
" TODO: sections with [[, ]]
nnoremap <buffer> <silent> ]o :set nohls<CR>/'\l\{2,\}'<CR>
nnoremap <buffer> <silent> [o :set nohls<CR>?'\l\{2,\}'<CR>
nnoremap <buffer> <silent> ]s :set nohls<CR>/\|\zs\S\+\ze\|<CR>
nnoremap <buffer> <silent> [s :set nohls<CR>?\|\zs\S\+\ze\|<CR>

nnoremap <buffer> <silent> q :q<CR>

