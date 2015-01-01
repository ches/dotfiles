" Project- or directory-specific Vim settings
"
" Summary of useful variables--see 'localvimrc-variables' for complete list.
"
"   - g:localvimrc_script_dir
"     Fully qualified directory of currently loaded .lvimrc

" -- Every BufEnter --

" Guard multiple execution for currently edited file
if g:localvimrc_sourced_once_for_file
  finish
endif

" -- Once per file --

" Guard multiple execution for the running vim instance
if g:localvimrc_sourced_once
  finish
endif

" -- Once per Vim instance --
