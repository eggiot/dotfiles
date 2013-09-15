# Extract archives
extract () {
    if [ -f $1 ] ; then
        case $1 in
            *.tar.bz2)   tar xjf $1;;
            *.tar.gz)    tar xzf $1;;
            *.bz2)       bunzip2 $1;;
            *.rar)       rar x $1;;
            *.gz)        gunzip $1;;
            *.tar)       tar xf $1;;
            *.tbz2)      tar xjf $1;;
            *.tgz)       tar xzf $1;;
            *.zip)       unzip $1;;
            *.Z)         uncompress $1;;
            *.7z)        7z x $1;;
            *.rar)       unrar e $1;;
            *)           echo "'$1' cannot be extracted via extract()" ;;
        esac
    else
        echo "'$1' is not a valid file"
    fi
}

# execute $2 on any file that matches $1
filesexec () {
    find . -maxdepth 1 -type f -iname '*'"${1:-}"'*' -exec ${2:-file} {} \;  ;
}

alias fileexec="filesexec"

# Swap 2 filenames around.
function swap() {
    local TMPFILE=tmp.$$

    [ $# -ne 2 ] && echo "swap: 2 arguments needed" && return 1
    [ ! -e $1 ] && echo "swap: $1 does not exist" && return 1
    [ ! -e $2 ] && echo "swap: $2 does not exist" && return 1

    mv "$1" $TMPFILE
    mv "$2" "$1"
    mv $TMPFILE "$2"
}

# Creates an archive (*.tar.gz) from given directory.
function maketar() {
    tar cvzf "$1.tar.gz"  "$1";
}

# Create a ZIP archive of a file or folder.
function makezip() { zip -r "1.zip" "$1" ; }

# gets info about a file - basically a wrapper around stat
get_file_info()
{
    info=""
        case $1 in
            'owner')    info='%U';;
            'type')     info='%F';;
            'creation') info='%w';;
            'modified') info='%y';;
            'access')   info='%w';;
            'change')   info='%z';;
            'size')     info='%s';;
            'access')   info='%A';;
            *)          return 1;;
        esac

        stat -c $info $2 2>/dev/null
}