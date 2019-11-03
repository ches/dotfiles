BREW=`brew --prefix`

# Directory listings in Technicolor, the BSD/OS X way.
# Geoff Greer made a cool preview/generator for BSD and Linux:
# http://geoff.greer.fm/lscolors/
export CLICOLOR='true'
export LSCOLORS="gxfxcxdxbxegedabagacad"

# Set a modest Linux LS_COLORS because some tools like fd use it.
# See here for better: https://github.com/trapd00r/LS_COLORS
export LS_COLORS='no=00:fi=00:di=00;34:ln=00;36:pi=40;33:so=00;35:do=00;35:bd=40;33;01:cd=40;33;01:or=40;31;01:ex=00;32:*.tar=00;31:*.tgz=00;31:*.arj=00;31:*.taz=00;31:*.lzh=00;31:*.zip=00;31:*.z=00;31:*.Z=00;31:*.gz=00;31:*.bz2=00;31:*.deb=00;31:*.rpm=00;31:*.jar=00;31:*.jpg=00;35:*.jpeg=00;35:*.gif=00;35:*.bmp=00;35:*.pbm=00;35:*.pgm=00;35:*.ppm=00;35:*.tga=00;35:*.xbm=00;35:*.xpm=00;35:*.tif=00;35:*.tiff=00;35:*.png=00;35:*.mpg=00;35:*.mpeg=00;35:*.avi=00;35:*.fli=00;35:*.gl=00;35:*.dl=00;35:*.xcf=00;35:*.xwd=00;35:*.ogg=00;35:*.mp3=00;35:*.wav=00;35:*.tex=00;33:*.sxw=00;33:*.sxc=00;33:*.lyx=00;33:*.pdf=0;35:*.ps=00;36:*.asm=0;33:*.S=0;33:*.s=0;33:*.h=0;31:*.c=0;35:*.cxx=0;35:*.cc=0;35:*.C=0;35:*.o=0;30:*.am=0;33:*.py=0;34:'

# brew install lesspipe --with-syntax-highlighting
# It's a bit different from versions on common Linux distros
if installed lesspipe.sh; then
    export LESSOPEN="|lesspipe.sh %s"
fi

# https://github.com/joelthelion/autojump/wiki
# NOTE: edit the script to make "check custom install" location an `elif`, or
#   else it sources itself twice, and it's pretty slow. Should send fix upstream
#   https://github.com/wting/autojump/issues/590
if [ -r $BREW/etc/profile.d/autojump.sh ]; then
    . $BREW/etc/profile.d/autojump.sh
fi

# =================================
# =       App-specific Bits       =
# =================================

# NOTE: Set JAVA_HOME via `jenv enable-plugin export`
export GROOVY_HOME=/usr/local/opt/groovy/libexec
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
