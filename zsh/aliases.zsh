function changecolour () {
    sed "s/$2/$(tput setaf $1)$2$(tput sgr0)/"
}

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
alias debrmlock="sudo rm /var/lib/dpkg/lock; sudo dpkg --configure -a"

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
# find a running process
fp() {
    ps ax -o pid,command | grep $1 | sed -e "/[0-9] grep $1/d" -e "/[0-9] sed /d" | sed G | changecolour 2 $1
}
alias turnoff="sudo shutdown -h now" # shut down the computer
alias ff="ls -a | grep" # find file
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

rfind() {find -ls | grep $1 | sed "s/^.* //"}

# functionality changes
alias less="most"

# media
alias pinknoise="play -n synth pinknoise"
alias brownnoise="play -n synth brownnoise"
alias whitenoise="play -n synth whitenoise"
alias scrot="scrot -q 100"
alias groovesalad="mplayer -playlist http://somafm.com/groovesalad.pls"

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
alias aweb='wget --quiet --page-requisites --convert-links'

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
	    *.rar)       unrar e $1    ;;
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