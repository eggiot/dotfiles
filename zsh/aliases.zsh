##############
# MY ALIASES #
##############

source ~/dotfiles/zsh/aliases/pkg.zsh
source ~/dotfiles/zsh/aliases/suffixes.zsh
source ~/dotfiles/zsh/aliases/media.zsh
source ~/dotfiles/zsh/aliases/programming.zsh
source ~/dotfiles/zsh/aliases/search.zsh
source ~/dotfiles/zsh/aliases/security.zsh

## directory navigation
alias desktop="~/Desktop"
alias "..."="../.."
alias "...."="../../.."
alias "....."="../../../../"
alias 1tb="/media/1TB"

# sudoed commands
alias semacs="sudo emacs -nw"
alias sgedit="gksudo gedit"

# shortened
alias turnoff="sudo shutdown -h now" # shut down the computer
alias set-menu-button-layout="gconftool-2 --set /apps/metacity/general/button_layout --type string"
alias delete-thumbnails="find ~/.thumbnails -type f -exec rm {} \;"

# configuration
alias xorgconf="sudo emacs -nw /etc/X11/xorg.conf" # X server configuration

# fresh install
alias installprogramming="sudo aptitude install sbcl slime ipython python-numpy python-scipy codeblocks virtualbox-ose"
alias installoffice="sudo aptitude install fbreader lyx"
alias installmedia="sudo aptitude install vlc blender gimp xfburn audacity feh mplayer scrot jackd"
alias installnetworking="sudo aptitude install w3m chromium-browser"

# functionality additions
alias lsd="ls -ld *(-/DN)" # list only directories and symlinks to directories
alias lsh="ls -ld .*" # list only hidden files and directories

if [ "$TERM" != "dumb" ]; then
    eval "`dircolors -b`"
    alias ls='ls -F --color=auto' # colourful
    alias lsv='ls --color=auto --format=vertical' # vertical format
    alias lsl='ls --color=auto --format=long' # long format
fi

alias rmr="rm -R"

# functionality changes
alias less="most"

# git
alias gcm="git commit -a -m"
alias gra="git remote add"

# networking
alias ping="ping -c 3"
alias aweb='wget --quiet --page-requisites --convert-links'

# create directory and enter it
mdc() {mkdir -p "$1" && cd "$1"}

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