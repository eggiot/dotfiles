source ~/dotfiles/zsh/functions.zsh

red=1
green=2
yellow=3
fawn=3
blue=4
purple=5
pink=5
cyan=6
gray=7
white=8

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
source ~/dotfiles/zsh/aliases/file.zsh

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