# ~/.zshrc: Initialization for zsh interactive shell sessions.
# This is minimal for the time being, I'm not a regular zsh user.

# See `man zshcontrib` for this silliness.
unalias run-help
autoload run-help
alias help=run-help  # bash-like for builtins, easier to type

# This is correct for default macOS system installation currently.
# run-help is still useful if the var isn't set. Will generalize later.
if [ -d /usr/share/zsh/5.3/help ]; then
    HELPDIR=/usr/share/zsh/5.3/help
fi

if [ -d /usr/local/share/zsh-completions ]; then
    fpath=(/usr/local/share/zsh-completions $fpath)
fi

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# added by travis gem
[ -f ~/.travis/travis.sh ] && source ~/.travis/travis.sh
