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
alias ltr='ls -ltr'
alias l='ls -CF'

alias ..='cd ..'
alias ...='cd ../..'

alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
alias pcregrep='pcregrep --color=auto'

# History editing/repetition
alias r='fc -s'

# TODO: perhaps functions to detect and use servernames for CLI vs. GUI--
# tpope has examples
alias e='vim --remote-silent'
alias et='vim --remote-tab'
alias g='vim -g --remote-silent'
alias gt='vim -g --remote-tab'
alias p=$PAGER

# External IP
# ifcfg.me has some handy features, but this is faster and more reliable.
alias myip='dig +short myip.opendns.com @resolver1.opendns.com'

# Current time in ISO-8601
alias nowiso='date "+%Y-%m-%dT%H:%M:%S%z"'

# ====================================================
# =         App- and Platform-specific Bits          =
# ====================================================

if installed docker-machine; then
    alias dm='docker-machine'
fi
if installed docker-compose; then
    alias fig='docker-compose'
fi

#
# Version Control
#
if installed hg; then
    alias mq='hg -R $(hg root)/.hg/patches'
fi

if installed hub; then
    alias git=hub
fi

# Color for Ruby documentation
if installed ri; then
    alias ri='ri -f ansi'
fi

# Create a simple .rvmrc in the current dir using the current Ruby and gemset
if installed rvm; then
    alias mkrvmrc='echo "rvm `rvm-prompt i v g`" > .rvmrc'
fi

if installed bundle; then
    alias be='bundle exec'
fi

#
# OS X
#
if [ "$(uname -s)" == "Darwin" ]; then
    #
    # Remote Access
    #
    alias minissh='sshfs ches@kodama.local:/Users/ches /Volumes/sshfs/ches -oauto_cache,reconnect,volname="ches on kodama"'
    alias mediassh='sshfs ches@nausicaamedia.dyndns.org:/Volumes/Media /Volumes/sshfs/Media -oauto_cache,reconnect,volname="Media on kodama"'

    # Quick Look file
    # The wrapper script to qlmanage (in ~/bin) deals with stdin usage.
    alias ql='quicklook "$@"'

    # Options are not equivalent, but 90% use case is just find by file path
    alias locate='mdfind -name'
fi

