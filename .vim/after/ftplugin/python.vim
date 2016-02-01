if neobundle#is_installed('jedi-vim')
  nnoremap <buffer> <silent> gd :call jedi#goto_assignments()<CR>
  nnoremap <buffer> <silent> <LocalLeader>u :call jedi#usages()<CR>
  nnoremap <buffer> <silent> <LocalLeader>r :call jedi#rename()<CR>

  " TODO: preview window version, <C-w>}
  nnoremap <buffer> <silent> <LocalLeader><C-]> :call jedi#goto()<CR>
  nnoremap <buffer> <silent> <LocalLeader><C-w>]     :split<CR> :call jedi#goto()<CR>
  nnoremap <buffer> <silent> <LocalLeader><C-w><C-]> :split<CR> :call jedi#goto()<CR>

  nnoremap <buffer> <silent> <LocalLeader><C-v>] :vsplit<CR> :call jedi#goto()<CR>
  nnoremap <buffer> <silent> <LocalLeader><C-t>] :tab split<CR> :call jedi#goto()<CR>
endif

if neobundle#is_installed('vim-ipython')
endif

" Allow setting pdb breakpoints. Run file with !python % (current file)
" or substitute a path. F7 adds a breakpoint, Shift-F7 removes.
python << EOF
def SetBreakpoint():
    import re
    nLine = int(vim.eval('line(".")'))

    strLine = vim.current.line
    strWhite = re.search('^(\s*)', strLine).group(1)

    vim.current.buffer.append(
       "%(space)spdb.set_trace() %(mark)s Breakpoint %(mark)s" %
         {'space':strWhite, 'mark': '#' * 30}, nLine - 1)

    for strLine in vim.current.buffer:
        if strLine == "import pdb":
            break
    else:
        vim.current.buffer.append('import pdb', 0)
        vim.command('normal j1')

# TODO: function to remove only current line's breakpoint instead of all
def RemoveBreakpoints():
    import re

    nCurrentLine = int(vim.eval('line(".")'))

    nLines = []
    nLine = 1
    for strLine in vim.current.buffer:
        if strLine == 'import pdb' or strLine.lstrip()[:15] == 'pdb.set_trace()':
            nLines.append(nLine)
        nLine += 1

    nLines.reverse()

    for nLine in nLines:
        vim.command('normal %dG' % nLine)
        vim.command('normal dd')
        if nLine < nCurrentLine:
            nCurrentLine -= 1

    vim.command('normal %dG' % nCurrentLine)

# TODO: I don't especially like this choice of key; look at debugger plugins
vim.command('nmap <buffer> <F8> :py SetBreakpoint()<CR>')
vim.command('nmap <buffer> <S-F8> :py RemoveBreakpoints()<CR>')
EOF

