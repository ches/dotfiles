setlocal linebreak
setlocal spell
setlocal omnifunc=htmlcomplete#CompleteTags

" Create headings
nnoremap <buffer> <leader>1 yypVr=
nnoremap <buffer> <leader>2 yypVr-
nnoremap <buffer> <leader>3 m`I###<Space><Esc>A<Space>###<Esc>``4l
nnoremap <buffer> <leader>4 m`I####<Space><Esc>A<Space>####<Esc>``5l

" Octodown faithfully renders Github-Flavored Markdown with Github's styles.
" $ gem install octodown
if executable('octodown')
  if has('mac')
    " Redirecting to /dev/null because qlmanage spews some spurious junk.
    setlocal makeprg=octodown\ %\ --raw\ \\\|\ quicklook\ -\ >&\ /dev/null

    " Set directly because Dispatch doesn't seem to take the IO redirection
    " from makeprg
    let b:dispatch = 'octodown % --raw | quicklook - >& /dev/null'
  else
    setlocal makeprg=octodown\ %
  endif
endif

