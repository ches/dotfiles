# ~/.bashrc: Initialization for bash interactive shell sessions.

# Simple check for an interactive shell -- don't do anything else if not.
[ -z "$PS1" ] && return

# The Google Shell Style Guide contains some tips for sane bash practices:
#    http://google.github.io/styleguide/shell.xml
#    Also: https://wiki.bash-hackers.org/
#
# Bash features and builtins like pattern matching and Parameter Expansion are
# probably _more_ portable at this point than shelling out to commands, as one
# soon finds with GNU sed versus BSD on macOS, etc. They're still ugly, though.

OSNAME="$(uname -s)"

#
# Bash history
#
bind Space:magic-space  # auto-expand history magic
shopt -s histappend     # make sure hist is kept across sessions
export HISTCONTROL=erasedups
export HISTIGNORE="jobs:bg:fg:cd:cd -:cd ..:clear:date:exit:w:ls:l:ll"
export PROMPT_COMMAND='history -a'  # Append immediately so new shells can use it

shopt -s cdable_vars    # bash-completion gives pushd CDPATH completion like cd
shopt -s checkwinsize   # update window size vars after each command

#
# Bash-specific setting env vars
#

# Please don't autocomplete these thx
export FIGNORE="#:~:DS_Store:.pyc:.swp:.swo"

export FCEDIT=vim

# Nix flow control to free the keys for readline inc search, vim, etc.
stty stop undef
stty start undef

# 'cd' to children of a host of directories, as if they were always in CWD
export CDPATH=:~:~/src/work:~/src

# A man's prompt is his castle, or something.
[[ -r ~/.bash.d/prompt.sh ]] && source ~/.bash.d/prompt.sh

# Functions for great justice
[[ -r ~/.bash.d/functions.sh ]] && source ~/.bash.d/functions.sh

# And aliases for all mankind
[[ -r ~/.bash.d/aliases.sh ]] && source ~/.bash.d/aliases.sh

#-------------------------------------------------------------------------------
# App- and Platform-specific Bits
#-------------------------------------------------------------------------------

if [ "$OSNAME" == "Darwin" ] && [ -r ~/.bash.d/platform-osx.sh ]; then
    . ~/.bash.d/platform-osx.sh
elif [ -r ~/.bash.d/platform-linux.sh ]; then
    . ~/.bash.d/platform-linux.sh
fi

# AWS credential management and env vars that the API tools want
[[ -r ~/.aws/setup.sh ]] && source ~/.aws/setup.sh

# Travis CI CLI
[[ -r ~/.travis/travis.sh ]] && source ~/.travis/travis.sh

#-------------------------------------------------------------------------------
# Language packaging, sandboxes, and stuff
#-------------------------------------------------------------------------------

# Localized environment variable automation -- http://direnv.net/
# brew install direnv
# OPTIMIZE: jenv, pyenv, chruby inits are slow; consider replacing them with direnv
installed direnv && eval "$(direnv hook bash)"

installed jenv && eval "$(jenv init -)"

# OCaml OPAM configuration
[[ -r ~/.opam/opam-init/init.sh ]] && source ~/.opam/opam-init/init.sh

# Python Version Switching & virtualenvs
# brew install pyenv pyenv-virtualenv
installed pyenv && eval "$(pyenv init -)"
installed pyenv-virtualenv-init && eval "$(pyenv virtualenv-init -)"

# Ruby Version Switching
# brew install chruby ruby-install
# Well put: http://pbrisbin.com/posts/chruby
if [ -x /usr/local/opt/chruby ]; then
    source /usr/local/opt/chruby/share/chruby/chruby.sh
    source /usr/local/opt/chruby/share/chruby/auto.sh
fi

#-------------------------------------------------------------------------------
# Machine-specific stuff, creds kept out of SCM, etc.
#-------------------------------------------------------------------------------
if [ -d ~/.local ]; then
    for f in ~/.local/*.sh; do
        . "$f"
    done
fi

# Completion - source late for scripts that check for commands added to PATH above
[[ -r ~/.bash.d/completion.sh ]] && source ~/.bash.d/completion.sh

unset OSNAME
