# ~/.bash_profile: executed by bash(1) for login shells.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# The way things are supposed to work:
#   1) bash loads this for login shells
#      a) we delegate to .profile for that
#   2) we load .bashrc, for when the login shell is interactive. bash only does
#      it for NON-login interactive shells.
#   3) .bashrc guards if shell is non-interactive, so if we source it here from
#      a non-interactive login shell, it just skips.
#
# Beware, though, that macOS is weird and Terminal.app starts login shells by
# default with login(1), so iTerm and others followed suit. That is the case for
# tmux too, since sessions may be re-attached across different logins.
#
# Some Linux distros are known to build bash with options that cause it to load
# .bashrc for non-interactive shells by default. Ugh.
#
# https://superuser.com/a/789465/79564
#
# Helpful for startup profiling: https://stackoverflow.com/a/5015179/455009

# Load common stuff for a login shell.
[ -r ~/.profile ] && . ~/.profile

# Also load stuff for interactive (login) shells.
[ -r ~/.bashrc ] && . ~/.bashrc

# In keeping with tradition.
#
# For a random cow. But coreutils on macOS, ugh.
# cowsay -f `ls -1 /usr/local/share/cows/*.cow | sort -R | head -1`
if [[ $- == *i* ]]; then  # is interactive
    if installed fortune; then
        if installed cowsay; then
            fortune -s | cowthink -n
        else
            fortune -s
        fi
    fi
fi
