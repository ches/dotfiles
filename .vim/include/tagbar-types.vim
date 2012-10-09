" I hate having these ugly hash literals in my main vimrc.

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

let g:tagbar_type_markdown = {
  \ 'ctagstype' : 'markdown',
  \ 'kinds' : [
    \ 'h:Heading1',
    \ 'i:Heading2',
    \ 'k:Heading3'
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

let g:tagbar_type_scala = {
  \ 'ctagstype' : 'Scala',
  \ 'kinds'     : [
    \ 'p:packages:1',
    \ 'V:values',
    \ 'v:variables',
    \ 'T:types',
    \ 't:traits',
    \ 'o:objects',
    \ 'a:aclasses',
    \ 'c:classes',
    \ 'r:cclasses',
    \ 'm:methods'
  \ ]
\ }

let g:tagbar_type_vimwiki = { 'ctagstype' : 'vimwiki', 'kinds' : [ 'h:Headers' ] }

