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
set guifont=Monaco:h12

if has('mac')

  " Transparency
  set transp=4

  " Use full horizontal width on fullscreen
  " I really wish this could be toggled without entering and leaving fs mode --
  " I like the Writeroom-style central editor column when not using a vsplit
  map <F2> :set fuoptions+=maxhorz<CR>
  map <S-F2> :set fuoptions-=maxhorz<CR>

endif

