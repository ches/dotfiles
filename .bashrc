export LC_CTYPE=en_US.UTF-8
export MANWIDTH=80

#
# Bash history
#
export HISTCONTROL=erasedups
export HISTSIZE=5000
shopt -s histappend     # make sure hist is kept across sessions
bind Space:magic-space  # auto-expand history magic

# for the love of god don't offer to autocomplete this shit
export FIGNORE="#:~:DS_Store:.pyc:.swp:.swo"

#
# I think I'm a convert.
#
export EDITOR=vim
export VISUAL=vim

#
# PATH Settings, clearly
# Don't need any additions at the moment
#
# if [ -f ~/.bash.d/paths.sh ]; then
#     . ~/.bash.d/paths.sh
# fi

#
# Simple check for an interactive shell -- don't do anything else if not.
# So, make PATH additions and stuff before this.
#
[ -z "$PS1" ] && return

#
# 'cd' to children of a host of directories, as if they were always in CWD
#
export CDPATH=:~:~/src:~/src/work/railsmachine

#
# A man's prompt is his castle, or something.
#
if [ -f ~/.bash.d/prompt.sh ]; then
    . ~/.bash.d/prompt.sh
fi

#
# Directory listings in Technicolor
#
export CLICOLOR='true'
export LSCOLORS="gxfxcxdxbxegedabagacad"

#
# 'less' is more
#
export PAGER="/usr/bin/less"
export LESS="-Ri"
export LESSOPEN="|lesspipe.sh %s"

#
# Functions for great justice
#
if [ -f ~/.bash.d/functions.sh ]; then
	. ~/.bash.d/functions.sh
fi

#
# And aliases for all mankind
#
if [ -f ~/.bash.d/aliases.sh ]; then
    . ~/.bash.d/aliases.sh
fi

#
# Completion
#
if [ -f ~/.bash.d/completion.sh ]; then
    . ~/.bash.d/completion.sh
fi

# ====================================================
# =         App- and Platform-specific Bits          =
# ====================================================

#
# Startup file for Python's interactive interpreter.
# Sets up history and completion
#
export PYTHONSTARTUP=$HOME/.pythonrc
# virtualenv & wrapper
export VIRTUALENV_USE_DISTRIBUTE=true
export WORKON_HOME=$HOME/.virtualenvs
export PIP_VIRTUALENV_BASE=$WORKON_HOME
export PIP_RESPECT_VIRTUALENV=true

# Ruby Version Manager
[[ -s $HOME/.rvm/scripts/rvm ]] && source $HOME/.rvm/scripts/rvm
[[ -r $rvm_path/scripts/completion ]] && source $rvm_path/scripts/completion
PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting

export JRUBY_HOME=$HOME/.rvm/rubies/jruby-1.6.7.2

# AWS credential management and env vars that the Java API tools want
[[ -r ~/.aws/setup.sh ]] && source ~/.aws/setup.sh

#
# Memcached
#
export EVENT_NOKQUEUE=1

#
# Choose your flava
#
if [ "$(uname -s)" == "Darwin" ] && [ -f ~/.bash.d/platform-osx.sh ]; then
    . ~/.bash.d/platform-osx.sh
elif [ -f ~/.bash.d/platform-linux.sh ]; then
    . ~/.bash.d/platform-linux.sh
fi

# Machine-specific stuff, creds kept out of SCM, etc.
if [ -d ~/.local ]; then
    for f in ~/.local/*.sh; do
        . "$f"
    done
fi

