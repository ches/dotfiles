" Make help navigation easier on the hands.
nnoremap <buffer> <CR> <C-]>
nnoremap <buffer> <BS> <C-t>

" Navigate to next/previous option or subject.
" http://vim.wikia.com/wiki/Learn_to_use_help
nnoremap <buffer> <silent> ]o /'\l\{2,\}'<CR>
nnoremap <buffer> <silent> [o ?'\l\{2,\}'<CR>
nnoremap <buffer> <silent> ]s /\|\zs\S\+\ze\|<CR>
nnoremap <buffer> <silent> [s ?\|\zs\S\+\ze\|<CR>

