#
# Green working directory on it's own line.
#
# Sans git fanciness: PS1="\n\[\033[0;32m\]\w\[\033[0m\]\n[\u@\h]\$ "
#      without color: PS1="\n\w\n[\u@\h]\$ "
#
        RED="\[\033[01;31m\]"
     YELLOW="\[\033[01;33m\]"
      GREEN="\[\033[01;32m\]"
       BLUE="\[\033[01;34m\]"
  LIGHT_RED="\[\033[0;31m\]"
LIGHT_GREEN="\[\033[0;32m\]"
      WHITE="\[\033[1;37m\]"
 LIGHT_GRAY="\[\033[0;37m\]"
 COLOR_NONE="\[\033[0m\]"

function parse_git_branch {
    git_status="$(git status 2> /dev/null)"
    if [ -x git_status ]; then exit; fi
    git_stash="$(git stash list 2> /dev/null)"
    branch_pattern="^# On branch ([^${IFS}]*)"
    remote_pattern="# Your branch is (.*) of"
    diverge_pattern="# Your branch and (.*) have diverged"

    if [[ ! ${git_status} =~ "working directory clean" ]]; then
        state="${RED}⚡"
    fi
    if [[ -n ${git_stash} ]]; then
        stash="${RED}*"
    fi
    # add an else if or two here if you want to get more specific
    if [[ ${git_status} =~ ${remote_pattern} ]]; then
        if [[ ${BASH_REMATCH[1]} == "ahead" ]]; then
            remote="${YELLOW}↑"
        else
            remote="${YELLOW}↓"
        fi
    fi
    if [[ ${git_status} =~ ${diverge_pattern} ]]; then
        remote="${YELLOW}↕"
    fi
    if [[ ${git_status} =~ ${branch_pattern} ]]; then
        branch=${BASH_REMATCH[1]}
        echo " ${GREEN}(${WHITE}± ${branch}${stash}${state}${remote}${GREEN})"
    fi
}

# TODO: for speed, make only one `hg prompt` call and massage data
function hg_prompt_info {
    # Because my home dir is an hg repo for dotfiles...
    hg_root="$(hg root 2> /dev/null)"
    if [ -z ${hg_root} ] || [[ ${hg_root} = $HOME ]]; then exit; fi

    # Override the hg-prompt extensions status characters
    hg_status="$(hg prompt '{status}')"
    if [[ -n $hg_status ]]; then
        state="${RED}⚡"
    fi

    hg_shelf="$(hg attic 2> /dev/null)"
    if [[ -n $hg_shelf ]]; then
        shelf="${RED}*"
    fi

    # Imperfect, but I almost always want to have a 'master' bookmark
    hg_prompt="$(hg prompt '{{branch|quiet}:}{{bookmark}}{:{patch}}')"
    echo " ${GREEN}(${WHITE}☿ ${hg_prompt}${shelf}${state}${GREEN})"
}


function ruby_version {
    if [[ -f ~/.rvm/bin/rvm-prompt ]]; then
        local system=$(~/.rvm/bin/rvm-prompt s)
        local interp=$(~/.rvm/bin/rvm-prompt i)
        if [[ ! -n $system ]]; then
            # Don't show interpreter if it's just MRI
            case $interp in
                ruby) echo " ${RED}♦ $(~/.rvm/bin/rvm-prompt v g)${COLOR_NONE}" ;;
                *)    echo " ${RED}♦ $(~/.rvm/bin/rvm-prompt i v g)${COLOR_NONE}" ;;
            esac
        fi
    fi
}

function prompt_func {
    previous_return_value=$?;
    prompt="${GREEN}\w$(parse_git_branch)$(hg_prompt_info)${COLOR_NONE}$(ruby_version)\n[\u@\h]"
    if test $previous_return_value -eq 0
    then
        PS1="\n${prompt}\$ "
    else
        PS1="\n${prompt}${RED}\$${COLOR_NONE} "
    fi
}

export PROMPT_COMMAND=prompt_func
