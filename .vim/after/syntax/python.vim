" Give self special highlighting as many syntax highlighters do.
" https://github.com/hdima/python-syntax/pull/48
syn keyword pythonImport self

" Highlight docstrings as comments, they're very loud when strings.
" TODO: embed reST? Would be great to enable Ultisnips there.
syn region pythonDocstring  start=+^\s*[uU]\?[rR]\?"""+ end=+"""+ fold keepend excludenl contains=pythonEscape,@Spell,pythonDoctest,pythonDocTest2,pythonSpaceError
syn region pythonDocstring  start=+^\s*[uU]\?[rR]\?'''+ end=+'''+ fold keepend excludenl contains=pythonEscape,@Spell,pythonDoctest,pythonDocTest2,pythonSpaceError
hi link pythonDocstring pythonComment
