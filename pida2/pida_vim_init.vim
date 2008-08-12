:silent function! Bufferlist()
let i = 1
    let max = bufnr('$') + 1
    let lis = ""
    while i < max
        if bufexists(i)
            let lis = lis.";".i.":".bufname(i)
        endif
        let i = i + 1
    endwhile
    return lis
:endfunction
:silent function! BreakPoint(l)
    call Async_event(v:servername.":set_breakpoint,".a:l)
:endfunction
:silent function! Yank_visual()
    y
    return @"
:endfunction
:silent function! Async_event(e)
    let args = substitute(a:e, ",", "", "g")
    let c = "silent call server2client('".expand('<client>')."', '".args."')"
    try
        exec c
    catch /.*/
        echo c
    endtry
:endfunction
:silent function! Pida_Started()
    silent call Async_event(v:servername.":filesave,")
    echo "PIDA connected"
:endfunction
:silent sign define break text=!B
:silent augroup pida
:silent set guioptions-=T
:silent set guioptions-=m
:silent au! pida
:silent au pida BufEnter * silent call Async_event(v:servername.":bufferchange,".getcwd().",".bufname('%').",".bufnr('%'))
:silent au pida BufDelete * silent call Async_event(v:servername.":bufferunload,".expand('<amatch>'))
:silent au pida VimLeave * silent call Async_event(v:servername.":shutdown,")
:silent au pida VimEnter * silent call Pida_Started()
:silent au pida BufWritePost * silent call Async_event(v:servername.":filesave,")
:silent au pida CursorMovedI * silent call Async_event(v:servername.":cursor_move,".line('.'))
:silent au pida CursorMoved * silent call Async_event(v:servername.":cursor_move,".line('.'))

:silent function! Pida_Complete(findstart, base)
    " locate the start of the word
    let line = getline('.')
    let start = col('.') - 1
    while start > 0 && line[start - 1] =~ ''
        let start -= 1
    endwhile
    if a:findstart
        let g:completing = 1
	    return start
    else
        call Async_event(v:servername.":complete".a:findstart."".a:base."".line."".start)
        let completion_time = 0
        while g:completing && completion_time < 500
            sleep 100m
            let completion_time = completion_time + 100
            "if complete_check()
            "    break
            "endif
        endwhile
        return []
    endif
:endfunction

:silent function! Pida_Stop_Completing()
    let g:completing = 1
:endfunction
set completefunc=Pida_Complete
