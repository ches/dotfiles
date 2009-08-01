" Author:      Michael Sanders (msanders42 [at] gmail [dot] com)
" Description: Slate colorscheme, adopted from TextMate.
" Usage:       This colorscheme is meant only for use with vim in Terminal.app or
"              gvim. I haven't tested it in other terminals. To get it to work in
"              Terminal.app, first install TerminalColors
"              (http://www.culater.net/software/TerminalColors/TerminalColors.php)
"              and use this theme I made to go along with this color scheme:
"              http://msanders.homeip.net/Slate.terminal

" The following are the preferred 16 colors for Terminal.app:
"
"           Colors      Bright Colors
" Black     #4E4E4E     #7C7C7C
" Red       #FF6C60     #EF5860
" Green     #A8FF60     #CEFFAB
" Yellow    #FFFFB6     #FFFFCB
" Blue      #96CBFE     #FFFFCB
" Magenta   #FF73FD     #FF9CFE
" Cyan      #C6C5FE     #DFDFFE
" White     #EEEEEE     #FFFFFF

" These are the modified colors for this theme:
" 			Colors		Bright Colors
" Black		#0e2231		#afb2ba (#0e2231 is used solely for highlighting lines)
" Red		#ed1600		#fc5a56
" Green		#55e439		#84f796
" Yellow	#eceb00		#f5f16e
" Blue		#009be7		#84ffff
" Magenta	#ffa705		#f1994a (magenta == orange now)
" Cyan		#8696aa		#abc4dd
" White		#f8f8f8		#ffffff

set bg=dark
hi clear
let colors_name = 'slate'

" General colors
hi Normal		guifg=#f8f8f8	guibg=#12384b gui=none
hi NonText		guifg=#84ffff	gui=none					ctermfg=blue

hi Cursor		guifg=NONE		guibg=#8ba7a7
hi LineNr		guifg=#afb2ba	gui=bold					ctermfg=darkgray				cterm=bold

hi VertSplit	guifg=#f8f8f8	guibg=#0e2231	gui=none	ctermfg=gray	ctermbg=black	cterm=none

hi Visual		guibg=#abc4dd	ctermbg=cyan

hi Title		guifg=#f1994a	guibg=NONE		ctermfg=magenta ctermbg=NONE
hi WildMenu		guifg=#0e2231	guibg=#afb2ba	ctermfg=black	ctermbg=gray
hi ErrorMsg		guifg=#f8f8f8	guibg=#ed1600	gui=bold		ctermfg=gray	ctermbg=darkred	cterm=bold
hi WarningMsg	guifg=#f5f16e	guibg=#0e2231	gui=bold		ctermfg=yellow	ctermbg=black   cterm=bold

hi ModeMsg		guifg=#afb2ba	guibg=#0e2231	gui=bold		ctermfg=gray	ctermbg=black	cterm=bold

if version >= 700 " Vim 7 specific colors
  hi CursorLine		guibg=#0e2231									ctermbg=black	cterm=none
  hi! link CursorColumn CursorLine
  hi MatchParen		guifg=#0e2231	guibg=#afb2ba	ctermfg=black	ctermbg=gray
  hi Search			guifg=NONE		guibg=NONE		gui=inverse		ctermfg=none	ctermbg=none	cterm=inverse
en

hi Pmenu			guifg=#000000	guibg=#f8f8f8				ctermfg=black		ctermbg=gray
hi PmenuSbar		guifg=#8696aa	guibg=#f8f8f8	gui=none	ctermfg=darkcyan	ctermbg=gray	 cterm=none
hi PmenuThumb		guifg=#f8f8f8	guibg=#8696aa	gui=none	ctermfg=gray		ctermbg=darkcyan cterm=none

" Syntax highlighting
hi Comment			guifg=#009be7	gui=italic		ctermfg=darkblue
hi String			guifg=#55e439					ctermfg=darkgreen

hi Keyword			guifg=#ffa705					ctermfg=darkmagenta
hi PreProc			guifg=#f1994a					ctermfg=magenta

hi Todo				guifg=#abc4dd	guibg=NONE		ctermfg=cyan	ctermbg=none
hi Constant			guifg=#fa6870					ctermfg=red

hi Identifier		guifg=#f1994a					ctermfg=magenta	cterm=none
hi Type				guifg=#f5f16e	gui=none		ctermfg=yellow
hi Statement		guifg=#f1994a	gui=none		ctermfg=magenta

hi Special			guifg=#84f796					ctermfg=green
hi Delimiter		guifg=#f1994a	gui=none		ctermfg=magenta

hi  link Number         Constant
hi! link StatusLine     VertSplit
hi! link StatusLineNC   VertSplit
" hi! link Identifier     Function
hi! link Question       Special
hi! link MoreMsg        Special
hi! link Folded         Normal

hi link Operator        Delimiter
hi link Function        Identifier
hi link PmenuSel        PmenuThumb
hi link Error			ErrorMsg
hi link Conditional		Keyword
hi link Character		String
hi link Boolean			Constant
hi link Float			Number
hi link Repeat			Statement
hi link Label			Statement
hi link Exception		Statement
hi link Include			PreProc
hi link Define			PreProc
hi link Macro			PreProc
hi link PreCondit		PreProc
hi link StorageClass	Type
hi link Structure		Type
hi link Typedef			Type
hi link Tag				Special
hi link SpecialChar		Special
hi link SpecialComment	Special
hi link Debug			Special

" Ruby
hi link rubyClass				Keyword
hi link rubyModule				Keyword
hi link rubyKeyword				Keyword
hi link rubyOperator			Operator
hi link rubyIdentifier			Identifier
hi link rubyInstanceVariable	Identifier
hi link rubyGlobalVariable		Identifier
hi link rubyClassVariable		Identifier
hi link rubyConstant			Type

" HTML/XML
hi link xmlTag				HTML
hi link xmlTagName			HTML
hi link xmlEndTag			HTML
hi link htmlTag				HTML
hi link htmlTagName			HTML
hi link htmlSpecialTagName	HTML
hi link htmlEndTag			HTML
hi link HTML 				NonText

" JavaScript
hi link javaScriptNumber	Number

" Obj-C
hi link objcDirective		Type

" CSS
hi link cssBraces			Normal
hi link cssTagName			NonText
hi link StorageClass		Special
hi link cssClassName		Special
hi link cssIdentifier		Identifier
hi link cssColor			Type
hi link cssValueInteger		Type
hi link cssValueNumber		Type
hi link cssValueLength		Type
hi cssPseudoClassId guifg=#eceb00 ctermfg=darkyellow

hi clear SpellBad
hi SpellBad ctermfg=red term=underline cterm=underline
hi clear SpellCap
hi SpellCap term=underline cterm=underline
hi clear SpellRare
hi SpellRare term=underline cterm=underline
hi clear SpellLocal
hi SpellLocal term=underline cterm=underline
" vim:noet:sw=4:ts=4:ft=vim
