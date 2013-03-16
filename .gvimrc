set guioptions-=T       " Toolbar off
set guioptions-=L       " No left hand scrollbars

" Window size - automatically larger for vimdiff!
if &diff
  set columns=170 lines=50

  " So many schemes have forgotten diff highlighting...
  colorscheme xoria256
else
  set columns=100 lines=50
end

" Font
" I can't really be bothered with automating all this special setup :-/
set guifont=Inconsolata\ for\ Powerline:h14

if has('mac')

  " Transparency
  set transp=4

  " Use full horizontal width on fullscreen
  " I really wish this could be toggled without entering and leaving fs mode --
  " I like the Writeroom-style central editor column when not using a vsplit
  set fuoptions+=maxhorz
  function! ToggleMaxhorz()
    if &fuoptions =~ 'maxhorz'
      set fuoptions-=maxhorz
      echo 'maxhorz off'
    else
      set fuoptions+=maxhorz
      echo 'maxhorz on'
    endif
  endfunction
  map <M-F2> :call ToggleMaxhorz()<CR>

endif

