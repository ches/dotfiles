#
# Completion
#

if [ -f /etc/bash_completion ]; then
    source /etc/bash_completion
elif [ -f /usr/local/etc/bash_completion ]; then # OS X Homebrew
    source /usr/local/etc/bash_completion
fi

source ~/.bash.d/completion-django.sh
source ~/.bash.d/completion-knife.sh
source ~/.bash.d/completion-git.sh
source ~/.bash.d/completion-hg.sh
source ~/.bash.d/completion-VBoxManage.sh

if installed sake; then
    complete -W "$(sake -T | awk {'print $2'})" sake
fi

#
# Rake
#
complete -C ~/.bash.d/completion-rake.rb -o default rake

# Viable alternative, a bit slower without the cache file.
# Props to Clinton Nixon for rake and thor:
# http://quiteuseless.net/crnixon/knapsack/blob/master/bashrc

# export COMP_WORDBREAKS=${COMP_WORDBREAKS/\:/}
# _rakecomplete() {
#   COMPREPLY=($(compgen -W "`rake -s -T | awk '{{print $2}}'`" -- ${COMP_WORDS[COMP_CWORD]}))
#   return 0
# }
# complete -o default -o nospace -F _rakecomplete rake

#
# Thor
#
# FIXME: this isn't quite working -- a control char is being choked on or something
_thorcomplete() {
    COMPREPLY=($(compgen -W "`thor -T | pcregrep -v -M "^.+\n^\-+|^$" | grep -v "*" | \
        awk '{{print $1}}'`" -- ${COMP_WORDS[COMP_CWORD]}))
    return 0
}
complete -o default -o nospace -F _thorcomplete thor

#
# SSH
#
#SSH_KNOWN_HOSTS=( $(cat ~/.ssh/known_hosts | \
#  cut -f 1 -d ' ' | \
#  sed -e s/,.*//g | \
#  uniq | \
#  egrep -v [0123456789]) )
SSH_CONFIG_HOSTS=( $(cat ~/.ssh/config | grep "Host " | grep -v "*" | cut -f 2 -d ' ') )
 
complete -o default -W "${SSH_CONFIG_HOSTS[*]}" ssh

# enable command and file completion after sudo
complete -f -c sudo
