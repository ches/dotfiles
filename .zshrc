# See `man zshcontrib` for this silliness.
unalias run-help
autoload run-help
alias help=run-help  # bash-like for builtins, easier to type

# This is correct for default OS X system installation currently.
# run-help is still useful if the var isn't set.
if [ -d /usr/share/zsh/5.0.5/help ]; then
    HELPDIR=/usr/share/zsh/5.0.5/help
fi

if [ -d /usr/local/share/zsh-completions ]; then
    fpath=(/usr/local/share/zsh-completions $fpath)
fi

# added by travis gem
[ -f /Users/ches/.travis/travis.sh ] && source /Users/ches/.travis/travis.sh
eval $(/usr/libexec/path_helper -s)

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
