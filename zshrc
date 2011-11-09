# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="edvardm" #muse

# Set to this to use case-sensitive completion
CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git command-not-found python)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
export PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games

############
# MY SETUP #
############
if ! [ $TERM ] ; then
    eval `tset -s -Q`
    case $TERM in
      con*|vt100) tset -Q -e ^?
        ;;
    esac
fi



##############
# MY ALIASES #
##############
# package management
alias pkg="aptitude"
alias spkg="sudo aptitude"
alias spkgi="sudo aptitude -P install"
alias spkgu="sudo aptitude update; sudo aptitude safe-upgrade"
alias spkgsu="sudo aptitude safe-upgrade"
alias spkgr="sudo aptitude -P remove"
alias spkgc="sudo aptitude clean"
alias pkgs="aptitude search"
alias pkgsh="aptitude show"
alias pkgd="aptitude download"
alias pkgsf="apt-cache-formatted"

alias debinstall="sudo dpkg -i"
alias debunpack="dpkg --unpack"
alias debconfigure="dpkg --configure"
alias debremove="dpkg --remove"
alias debshow="dpkg-deb --show"

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
fp() {ps ax -o pid,command | grep $1 | sed G} # find a running process
alias turnoff="sudo shutdown -h now" # shut down the computer
alias alsa-lmms="pasuspender -- lmms" # start lmms using alsa
alias ff="ls -a | grep" # find file
alias set-menu-button-layout="gconftool-2 --set /apps/metacity/general/button_layout --type string"

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

rfind() {find -ls | grep $1 | sed "s/^.* //"}

# functionality changes
alias less="most"
#alias less="vim -u ~/.vimrc.pager -"
alias emacs="emacs -nw"

# media
alias pinknoise="play -n synth pinknoise"
alias brownnoise="play -n synth brownnoise"
alias whitenoise="play -n synth whitenoise"
alias scrot="scrot -q 100"

# git
alias gcm="git commit -a -m"
alias gra="git remote add"

# register specific applications for suffixes
alias -s png=feh
alias -s jpg=feh
alias -s jpeg=feh
alias -s html=w3m
alias -s org=w3m
alias -s avi=mplayer
alias -s flac=play
alias -s wav=play

# networking
alias ping="ping -c 3"

# programming

syscall() {w3m -no-graph ~/.linux-syscall.html | grep $1 | sed 's/|//g'}

# create directory and enter it
mdc() {mkdir -p "$1" && cd "$1"}

# Extract archives
extract () {
     if [ -f $1 ] ; then
         case $1 in
             *.tar.bz2)   tar xjf $1        ;;
             *.tar.gz)    tar xzf $1     ;;
             *.bz2)       bunzip2 $1       ;;
             *.rar)       rar x $1     ;;
             *.gz)        gunzip $1     ;;
             *.tar)       tar xf $1        ;;
             *.tbz2)      tar xjf $1      ;;
             *.tgz)       tar xzf $1       ;;
             *.zip)       unzip $1     ;;
             *.Z)         uncompress $1  ;;
             *.7z)        7z x $1    ;;
             *)           echo "'$1' cannot be extracted via extract()" ;;
         esac
     else
         echo "'$1' is not a valid file"
     fi
}

# search a man page for a pattern
manfind() {
    man $1 | grep $2
}

# wipe hard drive

security_wipe_hd () {
    # overwrite a drive with random data 7 times - for sensitive data
    for n in `seq 7`; do dd if=/dev/urandom of=$1 bs=8b conv=notrunc; done
}

make-password () {
    cat /dev/urandom | tr -dc 'a-zA-Z0-9' | fold -w $1 | head -n 1
}

# create a playlist / create sorted list of files
make-sorted-file-list () {
    find -maxdepth 1 -type f -iname \*$1 | sort > $2
}
# 256 colours
#export TERM=xterm-256color
