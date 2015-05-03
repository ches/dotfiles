BREW=`brew --prefix`

export VISUAL='mvim -f'

# https://github.com/joelthelion/autojump/wiki
if [ -r $BREW/etc/autojump.sh ]; then
    . $BREW/etc/autojump.sh
fi

# =================================
# =       App-specific Bits       =
# =================================

export DOCKER_HOST=tcp://192.168.59.103:2375  # for boot2docker
export DOCKER_TLS_VERIFY=1
export DOCKER_CERT_PATH=$HOME/.boot2docker/certs/boot2docker-vm

export JAVA_HOME=$(/usr/libexec/java_home -v 1.7.0)
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

unset BREW

