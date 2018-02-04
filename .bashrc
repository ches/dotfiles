# Simple check for an interactive shell -- don't do anything else if not.
[ -z "$PS1" ] && return

export LC_CTYPE=en_US.UTF-8
export MANWIDTH=80

#
# Bash history
#
bind Space:magic-space  # auto-expand history magic
shopt -s histappend     # make sure hist is kept across sessions
export HISTCONTROL=erasedups
export HISTSIZE=5000
export PROMPT_COMMAND='history -a'  # Append immediately so new shells can use it

shopt -s checkwinsize   # update window size vars after each command

# Please don't autocomplete these thx
export FIGNORE="#:~:DS_Store:.pyc:.swp:.swo"

#
# I think I'm a convert.
#
export EDITOR=vim
export VISUAL=gvim
export FCEDIT=vim

# PATH Settings, clearly
# Don't need any additions at the moment
# [[ -r ~/.bash.d/paths.sh ]] && source ~/.bash.d/paths.sh

# Nix flow control to free the keys for readline inc search, vim, etc.
stty stop undef
stty start undef

# 'cd' to children of a host of directories, as if they were always in CWD
export CDPATH=:~:~/src/work:~/src

# 'less' is more. lesspipe is loaded per platform.
export PAGER="/usr/bin/less"
export LESS="-Ri"

# A man's prompt is his castle, or something.
[[ -r ~/.bash.d/prompt.sh ]] && source ~/.bash.d/prompt.sh

# Functions for great justice
[[ -r ~/.bash.d/functions.sh ]] && source ~/.bash.d/functions.sh

# And aliases for all mankind
[[ -r ~/.bash.d/aliases.sh ]] && source ~/.bash.d/aliases.sh

# ====================================================
# =         App- and Platform-specific Bits          =
# ====================================================

if [ "$(uname -s)" == "Darwin" ] && [ -f ~/.bash.d/platform-osx.sh ]; then
    . ~/.bash.d/platform-osx.sh
elif [ -f ~/.bash.d/platform-linux.sh ]; then
    . ~/.bash.d/platform-linux.sh
fi

# AWS credential management and env vars that the API tools want
[[ -r ~/.aws/setup.sh ]] && source ~/.aws/setup.sh

# CLI fuzzy finder - https://github.com/junegunn/fzf
# brew install fzf
# /usr/local/opt/fzf/install --completion --key-bindings
[[ -r ~/.fzf.bash ]] && source ~/.fzf.bash

# Travis CI CLI
[[ -r ~/.travis/travis.sh ]] && source ~/.travis/travis.sh

#-------------------------------------------------------------------------------
# Language packaging, sandboxes, and stuff
#-------------------------------------------------------------------------------

# Localized environment variable automation -- http://direnv.net/
# brew install direnv
# TODO: consider dropping all the other lang-specific env tools that this obviates!
which direnv > /dev/null && eval "$(direnv hook bash)"

# Go default workspace
# $ mkdir -p ~/src/go/{bin,pkg,src}
export GOPATH=$HOME/src/go
export PATH=$PATH:$GOPATH/bin

# Haskell Cabal & Stack
[[ -d ~/.cabal/bin ]] && export PATH=~/.cabal/bin:$PATH
[[ -d ~/.local/bin ]] && export PATH=~/.local/bin:$PATH

# OCaml OPAM configuration
[[ -r ~/.opam/opam-init/init.sh ]] && source ~/.opam/opam-init/init.sh

# Python REPL startup file. Sets up history and completion.
export PYTHONSTARTUP=$HOME/.pythonrc

# Python Version Switching & virtualenvs
# brew install pyenv pyenv-virtualenv
which pyenv > /dev/null && eval "$(pyenv init -)"
which pyenv-virtualenv-init > /dev/null && eval "$(pyenv virtualenv-init -)"

# Ruby Version Switching
# brew install chruby ruby-install
# Well put: http://pbrisbin.com/posts/chruby
if [ -x /usr/local/opt/chruby ]; then
    source /usr/local/opt/chruby/share/chruby/chruby.sh
    source /usr/local/opt/chruby/share/chruby/auto.sh
fi

# Rust toolchain & cargo install binaries
# Use rustup for man pages, etc. e.g. `rustup man rustc`
# brew install rustup-init && rustup-init -y --no-modify-path
# brew install cargo-completion rustc-completion
# rustup completions bash > $(brew --prefix)/etc/bash_completion.d/rustup
[[ -d ~/.cargo/bin ]] && export PATH=~/.cargo/bin:$PATH

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
