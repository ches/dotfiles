" I hate having these ugly hash literals in my main vimrc.
"
" CtrlP sometimes needs custom types too, and it borrowed heavily from TagBar
" so the config is similar and kept here. See g:ctrlp_buftag_types

let g:ctrlp_buftag_types = {}

" Alternatively, this probably works better than regex defs in ~/.ctags:
"   https://github.com/lukaszkorecki/CoffeeTags
let g:tagbar_type_coffee = {
  \ 'ctagstype' : 'coffee',
  \ 'kinds'     : [
    \ 'c:classes',
    \ 'm:methods',
    \ 'f:functions',
    \ 'v:variables',
    \ 'f:fields',
  \ ]
\ }

let g:tagbar_type_go = {
  \ 'ctagstype' : 'go',
  \ 'ctagsbin'  : 'gotags',
  \ 'ctagsargs' : '-sort -silent',
  \ 'kinds'     : [
    \ 'p:package',
    \ 'i:imports:1',
    \ 'c:constants',
    \ 'v:variables',
    \ 't:types',
    \ 'n:interfaces',
    \ 'w:fields',
    \ 'e:embedded',
    \ 'm:methods',
    \ 'r:constructor',
    \ 'f:functions'
  \ ],
  \ 'sro' : '.',
  \ 'kind2scope' : {
    \ 't' : 'ctype',
    \ 'n' : 'ntype'
  \ },
  \ 'scope2kind' : {
    \ 'ctype' : 't',
    \ 'ntype' : 'n'
  \ }
\ }

let g:tagbar_type_markdown = {
  \ 'ctagstype' : 'markdown',
  \ 'kinds' : [
    \ 'h:Heading 1',
    \ 'i:Heading 2',
    \ 'k:Heading 3'
  \ ]
\ }

" In ctags trunk, but there hasn't been a release in 3 years...
"   brew install ctags --HEAD
let g:tagbar_type_objc = {
  \ 'ctagstype' : 'ObjectiveC',
  \ 'kinds'     : [
    \ 'i:interface',
    \ 'I:implementation',
    \ 'p:Protocol',
    \ 'm:Object_method',
    \ 'c:Class_method',
    \ 'v:Global_variable',
    \ 'F:Object field',
    \ 'f:function',
    \ 'p:property',
    \ 't:type_alias',
    \ 's:type_structure',
    \ 'e:enumeration',
    \ 'M:preprocessor_macro',
  \ ],
  \ 'sro'        : ' ',
  \ 'kind2scope' : {
    \ 'i' : 'interface',
    \ 'I' : 'implementation',
    \ 'p' : 'Protocol',
    \ 's' : 'type_structure',
    \ 'e' : 'enumeration'
  \ },
  \ 'scope2kind' : {
    \ 'interface'      : 'i',
    \ 'implementation' : 'I',
    \ 'Protocol'       : 'p',
    \ 'type_structure' : 's',
    \ 'enumeration'    : 'e'
  \ }
\ }

let g:tagbar_type_rspec = {
  \ 'ctagstype' : 'ruby',
  \ 'kinds' : [
    \ 'd:describe',
    \ 'C:context'
  \ ]
\ }

let g:tagbar_type_rust = {
  \ 'ctagstype' : 'rust',
  \ 'kinds' : [
    \'T:types',
    \'f:functions',
    \'g:enumerations',
    \'s:structures',
    \'m:modules',
    \'c:constants',
    \'t:traits',
    \'i:trait implementations',
  \ ]
\ }

let g:tagbar_type_vimwiki = { 'ctagstype' : 'vimwiki', 'kinds' : [ 'h:Headers' ] }

" Hat tip to haskell-vim-now
if executable('hasktags')
  let g:tagbar_type_haskell = {
    \ 'ctagsbin'  : 'hasktags',
    \ 'ctagsargs' : '--ctags --extendedctag --output=-',
    \ 'kinds'     : [
      \  'm:modules:0:1',
      \  'd:data: 0:1',
      \  'd_gadt: data gadt:0:1',
      \  't:type names:0:1',
      \  'nt:new types:0:1',
      \  'c:classes:0:1',
      \  'cons:constructors:1:1',
      \  'c_gadt:constructor gadt:1:1',
      \  'c_a:constructor accessors:1:1',
      \  'ft:function types:1:1',
      \  'fi:function implementations:0:1',
      \  'o:others:0:1'
    \ ],
    \ 'sro'        : '.',
    \ 'kind2scope' : {
      \ 'm' : 'module',
      \ 'c' : 'class',
      \ 'd' : 'data',
      \ 't' : 'type'
    \ },
    \ 'scope2kind' : {
      \ 'module' : 'm',
      \ 'class'  : 'c',
      \ 'data'   : 'd',
      \ 'type'   : 't'
    \ }
  \ }

  let g:ctrlp_buftag_types.haskell = {
    \ 'bin'  : 'hasktags',
    \ 'args' : '--ctags --extendedctag --output=-',
  \ }
endif

