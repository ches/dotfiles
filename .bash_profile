# ~/.bash_profile: executed by bash(1) for login shells.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

#umask 022

# OS X's PATH/MANPATH management with /etc/{man,}paths.d directories, which are
# often augmented by Homebrew Cask packages that include CLI tools. Needs to act
# before bashrc loads so that my personal path prepends like version managers
# take precedence.
if [ -x /usr/libexec/path_helper ]; then
    export MANPATH=${MANPATH:-} # path_helper won't adjust MANPATH unless one exists
    eval $(/usr/libexec/path_helper -s)
fi

# If this is an interactive shell, include .bashrc if it exists. OS X doesn't
# like to do this unless invoked with `bash -i`.
case $- in
    *i*)
        if [ -f ~/.bashrc ]; then
            . ~/.bashrc
        fi
esac

# set PATH so it includes user's private bin if it exists
if [ -d ~/bin ] ; then
    PATH=~/bin:"${PATH}"
fi

