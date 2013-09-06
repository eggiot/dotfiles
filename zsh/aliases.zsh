source ~/dotfiles/zsh/functions.zsh

##############
# MY ALIASES #
##############

source ~/dotfiles/zsh/aliases/pkg.zsh
source ~/dotfiles/zsh/aliases/suffixes.zsh
source ~/dotfiles/zsh/aliases/media.zsh
source ~/dotfiles/zsh/aliases/programming.zsh
source ~/dotfiles/zsh/aliases/search.zsh
source ~/dotfiles/zsh/aliases/security.zsh
source ~/dotfiles/zsh/aliases/git.zsh
source ~/dotfiles/zsh/aliases/net.zsh
source ~/dotfiles/zsh/aliases/ls.zsh
source ~/dotfiles/zsh/aliases/navigation.zsh

# shortened
alias turnoff="sudo shutdown -h now" # shut down the computer
alias set-menu-button-layout="gconftool-2 --set /apps/metacity/general/button_layout --type string"
alias delete-thumbnails="find ~/.thumbnails -type f -exec rm {} \;"

alias rmr="rm -R"

# functionality changes
alias less="most"

# create directory and enter it
mdc() {
    mkdir -p "$1" && cd "$1"
}

# Extract archives
extract () {
    if [ -f $1 ] ; then
        case $1 in
            *.tar.bz2)   tar xjf $1    ;;
            *.tar.gz)    tar xzf $1    ;;
            *.bz2)       bunzip2 $1    ;;
            *.rar)       rar x $1      ;;
            *.gz)        gunzip $1     ;;
            *.tar)       tar xf $1     ;;
            *.tbz2)      tar xjf $1    ;;
            *.tgz)       tar xzf $1    ;;
            *.zip)       unzip $1      ;;
            *.Z)         uncompress $1 ;;
            *.7z)        7z x $1       ;;
            *.rar)       unrar e $1        ;;
            *)           echo "'$1' cannot be extracted via extract()" ;;
        esac
    else
        echo "'$1' is not a valid file"
    fi
}