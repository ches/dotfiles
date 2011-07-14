# vim: ft=sh:ts=4:sw=4:autoindent:expandtab:
# Author: Avishai Ish-Shalom <avishai@fewbytes.com>

# We need to specify GNU sed for OS X, BSDs, etc.
if [[ "$(uname -s)" == "Darwin" ]]; then
    SED=gsed
else
    SED=sed
fi

# first argument set the command level
_get_knife_completions() {
    n=$1
    shift
    # first argument is knife, so shift it
    #[ "$1" == "knife" ] && shift
    local opts
    opts="$($@ --help | grep -E '^knife' | cut -f$n -d" " | grep -v -E '[][[:upper:].]+' |grep -v '(options)')"
    _upvar opts "$opts"
}

_flatten_knife_command() {
    echo ${words[*]} |${SED} -r -e "s/\ *${words[$cword]}\$//" -e 's/\W/_/g'
}
# Check cache file for category ( passed as $1 ) and run command if cache is empty
# Designed to be used with _get_knife_completions() and use the opts variables for options
_completion_cache() {
    local CACHE_DIR=${CHEF_HOME:-"$HOME/.chef"}/.completion_cache
    local flag COMMAND
    local OPTIND=1
    while getopts "c" flag "$@"; do
        case $flag in
        c)
            COMMAND=yes
            ;;
        *)
            ;;
        esac
    done
    shift $(( $OPTIND - 1 ))

    local CACHE_FILE="$CACHE_DIR/$1"
    shift
    if [ ! -f "$CACHE_FILE" ]; then
        if [[ "$COMMAND" == "yes" ]]; then
            opts=$( eval $@ )
        else
            $@
        fi
        [ -d "$CACHE_DIR" ] && echo $opts >"$CACHE_FILE"
    else
        opts=$(cat "$CACHE_FILE")
    fi
    _upvar opts "$opts"
}

_knife() {
    local opts cur prev cword words flattened_knife_command
    _completion_cache knife_commands _get_knife_completions 2 knife
    _get_comp_words_by_ref cur prev cword words
    flattened_knife_command=$(_flatten_knife_command)
    COMPREPLY=()
    case $flattened_knife_command in
    *knife_cookbook_upload|*knife_cookbook_test)
        local chef_repos
        if [[ -z $CHEF_REPOS ]]; then
            chef_repos=( $(${SED} -rn '/cookbook_path/ {s/.*\[(.*)\]/\1/g; s/[,'\'']//g; p}' ${CHEF_HOME:-"$HOME/.chef"}/knife.rb) )
        else
            chef_repos=( ${CHEF_REPOS[@]} )
        fi
        if [[ -n "$chef_repos" ]]; then
            opts=$( ls -1p ${chef_repos[@]} | sort -u | ${SED} -n 's/\/$//p' )
            COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
        fi
        ;;
    *knife_data)
        opts="bag"
        COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
        return 0
        ;;
    *knife_node_show|*knife_node_edit|*knife_node_delete|*knife_tag_*)
        _completion_cache -c knife_nodes "${words[0]} node list|${SED} -r -e 's/[\"\ ,]//g' -e '/[^0-9A-Za-z._-]+/d'"
        COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
        return 0
        ;;
    *knife_role_edit|*knife_role_show|*knife_role_delete)
        _completion_cache -c knife_roles "${words[0]} role list|${SED} -r -e 's/[\"\ ,]//g' -e '/[^0-9A-Za-z._-]+/d'"
        COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
        return 0
        ;;
    *knife_data_bag_delete|*knife_data_bag_show|*knife_data_bag_edit)
        _completion_cache -c knife_data_bags "${words[0]} data bag list|${SED} -r -e 's/[\"\ ,]//g' -e '/[^0-9A-Za-z._-]+/d'"
        COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
        return 0
        ;;
    *knife_data_bag_delete_*|*knife_data_bag_show_*|*knife_data_bag_edit_*)
        _completion_cache -c knife_data_bag_$prev "${words[0]} data bag show $prev 2>/dev/null|${SED} -r -e 's/[\"\ ,]//g' -e '/^[^0-9A-Za-z._-]+/d'"
        COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
        return 0
        ;;
    *knife_client_list|*knife_client_show|*knife_client_edit)
        _completion_cache -c knife_clients "${words[0]} client list|${SED} -r -e 's/[\"\ ,]//g' -e '/[^0-9A-Za-z._-]+/d'"
        COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
        return 0
        ;;
    *knife_environment_show|*knife_environment_edit|*knife_environment_delete)
        _completion_cache -c knife_environments "${words[0]} environment list|${SED} -r -e 's/[\"\ ,]//g' -e '/[^0-9A-Za-z._-]+/d'"
        COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
        return 0
        ;;
    *)
        case $cword in
        1)
            COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
            return 0
            ;;
        *)
            _completion_cache $flattened_knife_command  _get_knife_completions $(( $cword + 1 )) ${words[*]}
            COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
            return 0
            ;;
        esac
        ;;
    esac
    [[ ${#COMPREPLY[@]} -ge 1 ]] && return 0
}
complete -F _knife knife
