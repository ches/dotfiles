# Check if a command is installed. Supports shell functions, unlike `which`,
# which can be handy for things like chruby.
function installed {
    local cmd=$1
    command -v ${cmd} >/dev/null
}

# Run something, piping output through $PAGER.
# Nice for commands with --help and no man page, shell `help getotps`, etc.
function page {
    $* | $PAGER
}

# Run something, piping all output to null.
function silent {
    $* &> /dev/null
}

# Make a quick datetime-stamped copy of a file.
function mkbackup {
    local filename=$1
    local extension="${filename##*.}"
    local base="${filename%.*}"
    local filetime=$(date +%Y%m%d-%H%M%S)

    cp ${filename} ${base}-${filetime}.${extension}
}

# Colorize source code paging with less. Use bat or highlight if available, else
# fall back to less, hopefully with lesspipe coloring.
#
# Being super lazy about option handling here -- give options to less AFTER the
# file name positionally, e.g. `lessc somefile -N` for line numbering.
#
# Args:
#   filename (str): file to read with less
#   [lessopts]: optional options for less
#
# TODO:
#   Try to make lesspipe support this itself:
#   https://github.com/wofr06/lesspipe/issues/3
function lessc {
    local -r filename="$1"
    shift

    if installed bat; then
        bat --plain --paging=always --pager="less $@" "$filename"
    elif installed highlight; then
        highlight --force --out-format xterm256 --style xoria256 "$filename" | less "$@"
    elif installed pygmentize; then
        # lesspipe can use pygmentize, but not with options:
        # https://github.com/wofr06/lesspipe/issues/5
        # Also it's slow to enable as a default for plain `less`
        pygmentize -f terminal256 -P style=monokai -g "$filename" | less "$@"
    else
        less "$@" "$filename"
    fi
}

# fbr - checkout git branch with fzf
function fbr {
    if ! installed fzf; then
        echo 'fbr requires the fzf tool to be installed.'
        exit 1
    fi

    local branches branch
    branches=$(git branch -vv) \
        && branch=$(echo "$branches" | fzf +m) \
        && git checkout $(echo "$branch" | awk '{print $1}' | sed "s/.* //")
}

# Install or open home page for the selected application using brew.
# Displays an info quickview window for the currently marked application.
function ffbrew () {
    local token
    token=$(brew search | fzf --query="$1" +m --preview 'brew info {}')

    if [ -n "$token" ]
    then
        read -p "(i)nstall or open (h)omepage of $token: " input
        if [[ $input =~ ^[Ii] ]]; then
            brew install $token
        fi
        if [[ $input =~ ^[Hh] ]]; then
            brew home $token
        fi
    fi
}

function ffpv {
    fzf-tmux --preview-window right:70% \
        --preview '[[ $(file --mime {}) =~ binary ]] &&
                     echo {} is a binary file ||
                     (bat --color always {} ||
                      highlight -O ansi -l {} ||
                      cat {}) 2> /dev/null | head -500'
}

# autojump fuzzy finding
# normal autojump when used with arguments but invokes fzf prompt for plain `j`
function ffj {
    # TODO: maybe override default j function, move autojump setup so it isn't OS X-specific
    # unset -f j

    if [[ "$#" -ne 0 ]]; then
        cd $(autojump $@)
        return
    fi
    cd "$(autojump -s | gsed '/_____/Q; s/^[0-9,.:]*\s*//' |  fzf --height 40% --reverse --inline-info)"
}

# This expects a file path substring as argument, and feeds matching files below
# the current working directory into the fpp tool, which presents a selector UI
# where you can choose one or many of them to either open in EDITOR or run
# arbitrary commands on.
#
# Simple composition of good tools that do one thing well: ah, Unix.
# TODO: fzf instead!
function acton {
    if ! installed fpp; then
        echo 'acton requires the fpp tool to be installed.'
        exit 1
    fi

    local search=$1
    declare searchtool

    if installed ag; then
        searchtool=ag
    elif installed ack; then
        searchtool=ack
    else
        searchtool=find
    fi

    if [[ "$searchtool" == "find" ]]; then
        # Ugh, of course BSD and GNU find handle regex type differently.
        if [[ "$(uname -s)" == "Darwin" ]]; then
            # Shell escaping is always lovely.
            exec \
                find -E . -type f -regex '.*'"$search"'.*' \
                    -not -path './.git/*' -not -path './.hg/*' \
                | fpp
        else # assume GNU
            exec \
                find . -type f -regextype posix-extended -regex '.*'"$search"'.*' \
                    -not -path './.git/*' -not -path './.hg/*' \
                | fpp
        fi
    else
        exec "$searchtool" -g "$search" | fpp
    fi
}

#
# OS X
#
if [ "$(uname -s)" == "Darwin" ]; then
    # Mounts encrypted disk image containing bank statements, then creates a
    # new one including a new statement for a chosen account.
    #
    # TODO: possibly make non-interactive by using public key or certificate;
    # Secure way to download via cron and mechanize would rock :-)
    function encrypt_bank_statement {
        local ACCTS=( 'Citizens Checking' 'Citizens Savings' 'Orange Checking' )
        local STMT_PATH="$HOME/Documents/Business Docs/Financial/Bank Statements"

        # Handle input params
        local PS3='Which Account? '

        declare acct suffix new_statement

        select opt in "${ACCTS[@]}"; do
            case "$opt" in
                'Citizens Checking')
                    acct=$opt
                    suffix="citizens_checking"
                    new_statement=~/Downloads/temp.pdf
                    break
                    ;;
                'Citizens Savings')
                    acct=$opt
                    suffix="citizens_savings"
                    new_statement=~/Downloads/temp.pdf
                    break
                    ;;
                'Orange Checking')
                    acct=$opt
                    suffix="orange_checking"
                    new_statement=~/Downloads/eStatement.pdf
                    break
                    ;;
                *)
                    echo "Unknown account specificier. WTF."
                    ;;
            esac
        done

        # Attach the existing image and copy its contents; read/write images with encryption and good compression are a pain
        hdiutil attach -noautoopen "$STMT_PATH".dmg
        mkdir -p "$STMT_PATH"
        cp -pR /Volumes/Bank\ Statements/* "$STMT_PATH"

        # We're through with it now
        hdiutil detach /Volumes/Bank\ Statements

        # Time to actually move the newly downloaded statement
        if [ -f "$new_statement" ]; then
            mv "$new_statement" "$STMT_PATH/$acct/`date '+%Y%m'`-$suffix.pdf"
        else
            echo "Could not find newly downloaded statement at the expected location ($new_statement)."
            echo "I'm gonna shred the unencrypted statements directory now."
            srm -rfm "$STMT_PATH"
            return 1
        fi

        # We can safely delete the old image now to make room for the new one
        rm -f "$STMT_PATH".dmg

        # Finally, make the new image and securely delete the source directory
        echo "Creating new encrypted image..."
        if hdiutil create -encryption -stdinpass -srcfolder "$STMT_PATH" "$STMT_PATH".dmg; then
            echo "Removing unencrypted statements directory..."
            srm -rfm "$STMT_PATH"
            return 0
        else
            echo "Could not successfully create the new image. Statements directory left behind."
            return 1
        fi
    }
fi

