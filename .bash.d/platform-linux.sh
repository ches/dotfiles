# enable color support of ls and also add handy aliases
if [ "$TERM" != "dumb" ]; then
    eval "`dircolors -b`"
    alias ls='ls --color=auto'
    #alias dir='ls --color=auto --format=vertical'
    #alias vdir='ls --color=auto --format=long'
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

eval $('lesspipe')

# =================================
# =       App-specific Bits       =
# =================================

#
# Environment variables for Apple Shake
#
export NR_INCLUDE_PATH=~/nreal/plugins/Furnace3.0v1_shake4.00-linux-x86-release-32/include:~/nreal/plugins/Tinder2.1v1_Shake-v4.00_Linux/include
export NR_ICON_PATH=~/nreal/plugins/Furnace3.0v1_shake4.00-linux-x86-release-32/icons:~/nreal/plugins/Tinder2.1v1_Shake-v4.00_Linux/icons
export FOUNDRY_LICENSE_FILE=~/nreal/FLEXlm/foundry.lic
export FOUNDRY_LICENSE_LOG=~/nreal/FLEXlm/license.log

