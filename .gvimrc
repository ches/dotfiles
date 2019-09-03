set guioptions-=T       " Toolbar off
set guioptions-=L       " No left hand scrollbars

" Font - brew install homebrew/cask-fonts/font-fira-code
set guifont=Fira\ Code\ Light:h12

colorscheme base16-default-dark

" Many Airline themes are not at all 256-color friendly for console,
" fortunately much better in GUI Vim.
"
" Some nice combos:
"   customtwilight: ubaryd or molokai
"   atom: solarized
"   xoria256: molokai
"   Of course paired stuff: solarized, tomorrow, base16
let g:airline_theme = 'base16'

" Window size - automatically larger for vimdiff!
if &diff
  set columns=170 lines=50

  " So many schemes have forgotten diff highlighting...
  colorscheme xoria256
else
  set columns=100 lines=50
end

if has('mac')
  set macligatures
  set transparency=3
endif

