export EDITOR='mvim -f'
export VISUAL='mvim -f'

export FCEDIT=vim

# Nix flow control keys so readline inc search, emacs, etc. can have them
stty stop undef
stty start undef

# https://github.com/joelthelion/autojump/wiki
if [ -f `brew --prefix`/etc/autojump.sh ]; then
    . `brew --prefix`/etc/autojump.sh
fi

# =================================
# =       App-specific Bits       =
# =================================

export JAVA_HOME="/Library/Java/Home/"
export NODE_PATH="/usr/local/lib/node_modules"

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

