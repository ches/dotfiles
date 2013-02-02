# Check if a command is installed. For some reason that I can't remember, this
# is deemed superior to `which`.
function installed {
    local cmd=$1
    command -v ${cmd} >/dev/null
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

