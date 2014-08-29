BREW=`brew --prefix`

export EDITOR='mvim -f'
export VISUAL='mvim -f'

export FCEDIT=vim

# Nix flow control keys so readline inc search, emacs, etc. can have them
stty stop undef
stty start undef

# https://github.com/joelthelion/autojump/wiki
if [ -f $BREW/etc/autojump.sh ]; then
    . $BREW/etc/autojump.sh
fi

# =================================
# =       App-specific Bits       =
# =================================

export DOCKER_HOST=tcp://192.168.59.103:2375  # for boot2docker
export JAVA_HOME="/Library/Java/Home/"
export NODE_PATH="/usr/local/lib/node_modules"

[[ -r /usr/local/bin/virtualenvwrapper.sh ]] && source /usr/local/bin/virtualenvwrapper.sh

# Tell system Python 2.7 where Homebrew Python's modules are. This gets
# Mercurial plugins like hg-git installed with brew's pip to work.
export PYTHONPATH=$BREW/lib/python2.7/site-packages

#
# Custom Shake path
#
# export NR_SHAKE_LOCATION="/Applications/Digital Media/Video/Shake"

# iTerm Tab and Title Customization and prompt customization

# Put the string " [bash]   hostname::/full/directory/path"
# in the title bar using the command sequence
# \[\e]2;[bash]   \h::\]$PWD\[\a\]

# Put the penultimate and current directory 
# in the iterm tab
# \[\e]1;\]$(basename $(dirname $PWD))/\W\[\a\]

# Make a simple command-line prompt:  bash-$

# PS1=$'\[\e]2;[bash]   \h::\]$PWD\[\a\]\[\e]1;\]$(basename "$(dirname "$PWD")")/\W\[\a\]\u-\$ '

unset BREW

