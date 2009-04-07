#
# Path settings
#
# $HOME/bin is added in .bash_profile

# path_prepend () {
#     echo $PATH | /bin/egrep -q "(^|:)$1($|:)" || PATH=$1:$PATH
# }

# Not used for now with Leopard's path_helper thing...
# if [ "$(uname -s)" == "Darwin" ]; then
#     path_prepend "/Library/Frameworks/Python.framework/Versions/Current/bin"
#     path_prepend "/usr/local/bin"
#     path_prepend "/usr/local/mysql/bin"
#
#    # 
#    # Man pages seem to end up in stupid places sometimes on OS X
#    # 
#    export MANPATH=/usr/local/man:$MANPATH
# fi

# unset path_prepend
