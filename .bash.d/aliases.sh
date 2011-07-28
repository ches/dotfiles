#
# I like to protect myself from myself...
#
alias mv="mv -i"
alias cp="cp -i"
alias rm="rm -i"
alias ln="ln -i"

#
# Other handy aliases
#
alias h=history

alias ll="ls -laF"
alias la='ls -A'
alias lt='ls -lt'
alias l='ls -CF'

alias ..='cd ..'
alias ...='cd ../..'

alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
alias pcregrep='pcregrep --color=auto'

alias r='fc -s'

# TODO: perhaps functions to detect and use servernames for CLI vs. GUI--
# tpope has examples
alias e='vim --remote-silent'
alias et='vim --remote-tab'
alias g='vim -g --remote-silent'
alias gt='vim -g --remote-tab'
alias p=$PAGER

alias ri='ri -f ansi'

# ====================================================
# =         App- and Platform-specific Bits          =
# ====================================================

#
# Version Control
#
alias mq='hg -R $(hg root)/.hg/patches'

# Create a simple .rvmrc in the current dir using the current Ruby and gemset
alias mkrvmrc='echo "rvm `rvm-prompt i v g`" > .rvmrc'

#
# OS X
#
if [ "$(uname -s)" == "Darwin" ]; then
    # MacVim has all the juicy bits like ruby and python scripting support built in.
    # An alias instead of symlink means it loads all the vimruntime with no extra config.
    alias vim=/Applications/Development/MacVim.app/Contents/MacOS/Vim

    # Open Bitbucket page for the Mercurial repository the current working directory resides in
    alias bb='open $(hg paths | sed -En "s%^(.+) = (.+)bitbucket.org/(.+)/(.+)%https://bitbucket.org/\3/\4%p")'

    # Open Github page for the project and branch of the CWD
    # I use 'ghub' so avoid conflict with the handy github gem
    alias ghub='br=$(git branch --contains HEAD | sed -En "s/^\* //p"); if ! git ls-remote . | grep -q -e "refs/remotes/.*/${br}"; then br="master"; fi; open $(git config -l | sed -En "s%remote.origin.url=git(@|://)(github.com)(:|/)(.+/.+).git%https://\2/\4/tree/${br}%p")'

    #
    # Remote Access
    #
    alias minissh='sshfs ches@kodama.local:/Users/ches /Volumes/sshfs/ches -oauto_cache,reconnect,volname="ches on kodama"'

    # Leopard Quick Look:
    alias ql='qlmanage -p "$@" >& /dev/null'

    #alias top="top -X"  # the stupidly named 'compatibility mode' --
                         # DON'T mimic Jaguar, because Jaguar's top was stupid
fi

