# ~/.zprofile: executed by zsh(1) for login shells.
# zsh automatically loads zshrc if login shell is also interactive, unlike bash.

# Load common stuff for a login shell.
if [ -r ~/.profile ]; then
    . ~/.profile
fi
