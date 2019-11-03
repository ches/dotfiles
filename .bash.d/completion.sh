#
# Completion
#

# enable command and file completion after sudo
complete -f -c sudo

# Linux, or Homebrew on macOS, possibly with bash-completion@2 formula & bash 4+ ...
if [ -f /etc/bash_completion ]; then
    source /etc/bash_completion
elif [ -f /usr/local/etc/bash_completion ]; then
    source /usr/local/etc/bash_completion
elif [ "${BASH_VERSINFO}" -ge 4 ] && [ -f /usr/local/share/bash-completion/bash_completion ]; then
    source /usr/local/share/bash-completion/bash_completion
fi

for f in ~/.bash.d/completion-*.sh; do
    source $f
done

# From awscli
if installed aws_completer; then
    complete -C aws_completer aws
fi

# /usr/local/opt/fzf/install --completion --key-bindings
if [[ -r ~/.fzf.bash ]]; then
    source ~/.fzf.bash

    # Enable fzf ** completion on more commands
    complete -F _fzf_dir_completion -o default -o bashdefault tree
    complete -F _fzf_path_completion -o default -o bashdefault bat
fi

if installed kubectl; then
    source <(kubectl completion bash)
fi

# Stack build tool for Haskell
if installed stack; then
    eval "$(stack --bash-completion-script stack)"
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

# chruby allows fuzzy matching, but the listing is convenient.
# See evolution: https://github.com/postmodern/chruby/issues/27
__complete_chruby() {
    local cur rubie basenames
    typeset -a basenames
    cur="${COMP_WORDS[COMP_CWORD]}"
    for ruby in ${RUBIES[@]}; do
        basenames+=($(basename $ruby))
    done
    COMPREPLY=( $(compgen -W "${basenames[*]}" -- ${cur}) )
}
complete -F __complete_chruby chruby

#
# Aliases - apply bash_completion stuff for some shortened aliases I use
#
if installed _docker_compose; then
    complete -F _docker_compose fig
fi

if installed _docker_machine; then
    complete -F _docker_machine dm
fi

if installed _fzf_opts_completion; then
    complete -F _fzf_opts_completion ff
fi
