BREW=`brew --prefix`

export VISUAL='mvim -f'

# Directory listings in Technicolor, the BSD/OS X way.
# Geoff Greer made a cool preview/generator for BSD and Linux:
# http://geoff.greer.fm/lscolors/
export CLICOLOR='true'
export LSCOLORS="gxfxcxdxbxegedabagacad"

# brew install lesspipe --with-syntax-highlighting
# It's a bit different from versions on common Linux distros
if installed lesspipe.sh; then
    export LESSOPEN="|lesspipe.sh %s"
fi

# https://github.com/joelthelion/autojump/wiki
if [ -r $BREW/etc/autojump.sh ]; then
    . $BREW/etc/autojump.sh
fi

# =================================
# =       App-specific Bits       =
# =================================

export GROOVY_HOME=/usr/local/opt/groovy/libexec
export JAVA_HOME=$(/usr/libexec/java_home --version 1.8.0)
export NODE_PATH="/usr/local/lib/node_modules"

# Make paulp/sbt-extras pick up same default rc file as the `brew install sbt` runner.
if [ -r $BREW/etc/sbtopts ]; then
    export SBT_OPTS="@${BREW}/etc/sbtopts"
fi

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

unset BREW
