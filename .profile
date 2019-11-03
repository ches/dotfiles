# ~/.profile: Initialization for Bourne login shells.
#
# Must be compatible with a POSIX-compliant /bin/sh and is sometimes applied
# that way by display managers, affecting the environment of desktop application
# processes. POSIX shell specification:
#
#   https://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html
#
# I share stuff common between bash and zsh here when possible, though it should
# stay light for non-interactive shell invocations.

umask 022

set -a  # export all modified variables

LC_CTYPE=en_US.UTF-8

EDITOR=vim
unset VISUAL    # stay in the terminal

HISTSIZE=10000  # Used by both bash and zsh history
MANWIDTH=80
GREP_OPTIONS='--color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn}'
RIPGREP_CONFIG_PATH="$HOME/.config/rg/rc"

# 'less' is more. lesspipe is loaded per shell/platform.
PAGER='/usr/bin/less'
LESS='-Ri'

# macOS's PATH/MANPATH management with /etc/{man,}paths.d directories, which are
# often augmented by Homebrew Cask packages that include CLI tools. The default
# /etc/{z,}profile does this, but with MANPATH unset so it doesn't work.
#   https://scriptingosx.com/2017/05/where-paths-come-from/
if [ -x /usr/libexec/path_helper ]; then
    MANPATH="${MANPATH:-}" # path_helper won't adjust MANPATH unless one exists
    eval `/usr/libexec/path_helper -s`
fi

# Check if a command is installed, with builtins faster than `which`.
installed() { command -v "$1" >/dev/null 2>&1; }

# CLI fuzzy finder - https://github.com/junegunn/fzf
# $ brew install fzf
if installed fzf; then
    # ansi is slow, maybe not worth it for fd
    FZF_DEFAULT_OPTS='--ansi --border'

    # fd is a fast, user-friendly alternative to find - https://github.com/sharkdp/fd
    # brew install fd
    if installed fd; then
        # Enhance fzf (see below) to use fd
        FZF_DEFAULT_COMMAND='fd --type file --hidden --exclude .git --color=always'
        FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
    fi
    # TODO: try ag and rg too if fd not available
fi

# customize man page coloring (these are Arch theme)
# http://linuxtidbits.wordpress.com/2009/03/23/less-colors-for-man-pages/
LESS_TERMCAP_mb=$'\e[01;31m'       # begin blinking
LESS_TERMCAP_md=$'\e[01;38;5;74m'  # begin bold
LESS_TERMCAP_me=$'\e[0m'           # end mode
LESS_TERMCAP_so=$'\e[38;5;246m'    # begin standout-mode - info box
LESS_TERMCAP_se=$'\e[0m'           # end standout-mode
LESS_TERMCAP_us=$'\e[04;38;5;146m' # begin underline
LESS_TERMCAP_ue=$'\e[0m'           # end underline

#-------------------------------------------------------------------------------
# Language packaging, sandboxes, and stuff
#-------------------------------------------------------------------------------

# Go default workspace
# $ mkdir -p ~/src/go/{bin,pkg,src}
GOPATH="$HOME/src/go"
PATH="$GOPATH/bin:$PATH"

# Haskell Cabal & Stack
[ -d ~/.cabal/bin ] && PATH="$HOME/.cabal/bin:$PATH"
[ -d ~/.local/bin ] && PATH="$HOME/.local/bin:$PATH"

# Python REPL startup file. Sets up history and completion.
PYTHONSTARTUP="$HOME/.pythonrc"

# Rust toolchain & cargo install binaries
# Use rustup for man pages, etc. e.g. `rustup man rustc`
#   brew install rustup-init && rustup-init -y --no-modify-path
#   brew install cargo-completion rustc-completion
#   rustup completions bash > $(brew --prefix)/etc/bash_completion.d/rustup
[ -d ~/.cargo/bin ] && PATH="$HOME/.cargo/bin:$PATH"

# Prioritize personal bin over everything above, for wrapper scripts, etc.
[ -d ~/bin ] && PATH="$HOME/bin:$PATH"

set +a  # stop implicit variable exports
