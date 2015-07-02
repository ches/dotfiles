" Gitv has some mappings like <C-l> that clash with my window movement.
" They're disabled by a vimrc setting, now put back useful ones that don't clash.
nmap <buffer> <silent> <C-n> <Plug>(gitv-previous-commit)
nmap <buffer> <silent> <C-p> <Plug>(gitv-next-commit)
nmap <buffer> <silent> <C-CR> <Plug>(gitv-edit)

