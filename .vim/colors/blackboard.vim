" Vim color scheme
"
" Name:         blackboard.vim
" Maintainer:   Ben Wyrosdick <ben.wyrosdick@gmail.com> 
" Last Change:  2 July 2008
" License:      public domain
" Version:      1.1

set background=dark
hi clear
if exists("syntax_on")
   syntax reset
endif

let g:colors_name = "blackboard"

if has("gui_running")
  "GUI Colors
  highlight Normal guifg=White   guibg=#0B1022
  highlight Cursor guifg=Black   guibg=Yellow
  highlight CursorLine guibg=#191E2F
  highlight LineNr guibg=#323232 guifg=#888888
  highlight Folded guifg=White guibg=#1C3B79

  "General Colors
  highlight Comment guifg=#AEAEAE
  highlight Constant guifg=#CAFE1E
  highlight Keyword guifg=#FFDE00
  highlight String guifg=#00D42D
  highlight Type guifg=#84A7C1
  highlight Identifier guifg=#00D42D gui=NONE
  highlight Function guifg=#FF5600 gui=NONE
  highlight clear Search
  highlight Search guibg=#1C3B79
  highlight PreProc guifg=Grey
  
  " listchars stuff, extra line tildes
  highlight NonText       guifg=#404040 ctermfg=8
  highlight SpecialKey    guifg=#404040 ctermfg=8

  " Visual mode and stuff
  highlight Visual        guibg=#1C3B79
  highlight Pmenu         guifg=White ctermfg=White guibg=#1C3B79 gui=bold cterm=bold
  highlight PmenuSel      guifg=White ctermfg=White guibg=DarkCyan gui=bold cterm=bold

  "Ruby Colors
  highlight link rubyClass Keyword
  highlight link rubyDefine Keyword
  highlight link rubyConstant Type
  highlight link rubySymbol Constant
  highlight link rubyStringDelimiter rubyString
  highlight link rubyInclude Keyword
  highlight link rubyAttribute Keyword
  highlight link rubyInstanceVariable Normal

  "Rails Colors
  highlight link railsMethod Type
end
