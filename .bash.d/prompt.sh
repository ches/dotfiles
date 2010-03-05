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
        echo " ${GREEN}(${WHITE}${branch}${stash}${state}${remote}${GREEN})"
    fi
}

function prompt_func() {
    previous_return_value=$?;
    prompt="${GREEN}\w$(parse_git_branch)${COLOR_NONE}\n[\u@\h]"
    if test $previous_return_value -eq 0
    then
        PS1="\n${prompt}\$ "
    else
        PS1="\n${prompt}${RED}\$${COLOR_NONE} "
    fi
}

export PROMPT_COMMAND=prompt_func
