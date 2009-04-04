#
# I like to protect myself from myself...
#
alias mv="mv -i"
alias cp="cp -i"
alias rm="rm -i"

#
# Other handy aliases
#
alias ll="ls -laF"
alias la='ls -A'
alias l='ls -CF'

alias ..='cd ..'
alias ...='cd ../..'

# ====================================================
# =         App- and Platform-specific Bits          =
# ====================================================

#
# Version Control
#
alias mq='hg -R $(hg root)/.hg/patches'
alias bb='open $(hg paths | sed -En "s%^(.+) = (.+)bitbucket.org/(.+)/(.+)%https://bitbucket.org/\3/\4%p")'

# Open Github page for the project and branch of the CWD
# I use 'ghub' so avoid conflict with the handy github gem
alias ghub='br=$(git branch --contains HEAD | sed -En "s/^\* //p"); if ! git ls-remote . | grep -q -e "refs/remotes/.*/${br}"; then br="master"; fi; open $(git config -l | sed -En "s%remote.origin.url=git(@|://)(github.com)(:|/)(.+/.+).git%https://\2/\4/tree/${br}%p")'

#
# Remote Access
#
alias chessh='sshfs ches@Cinewave.local:/Users/ches /Volumes/sshfs/ches -oauto_cache,reconnect,volname="ches on Cinewave"'

#
# OS X
#

# OS X Leopard's Quick Look feature:
alias ql='qlmanage -p "$@" >& /dev/null'
#alias top="top -X"  # the stupidly named 'compatibility mode' --
                     # DON'T mimic Jaguar, because Jaguar's top was stupid

