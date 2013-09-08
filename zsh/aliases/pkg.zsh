# package management

# apt
alias pkg="aptitude"
alias spkg="sudo aptitude"
alias spkgi="sudo aptitude -P install"
alias spkgu="sudo aptitude update; sudo aptitude safe-upgrade"
alias spkgsu="sudo aptitude safe-upgrade"
alias spkgr="sudo aptitude -P remove"
alias spkgc="sudo aptitude clean"
alias pkgsh="aptitude show"
alias pkgd="aptitude download"
alias pkgsf="apt-cache-formatted"

pkgs () {
	aptitude search $1 | changecolour 2 $1
}

pkgsf () {
	apt-cache search $1 | changecolour 2 $1 #| cut -c -$(tput cols)

}
# deb
alias debinstall="sudo dpkg -i"
alias debunpack="dpkg --unpack"
alias debconfigure="dpkg --configure"
alias debremove="dpkg --remove"
alias debshow="dpkg-deb --show"
alias debrmlock="sudo rm /var/lib/dpkg/lock; sudo dpkg --configure -a"

debcontents () {
	dpkg -c $1 | strip_long_ls
}

# fresh install
alias installprogramming="sudo aptitude install sbcl slime ipython python-numpy python-scipy codeblocks virtualbox-ose"
alias installoffice="sudo aptitude install fbreader lyx"
alias installmedia="sudo aptitude install vlc blender gimp xfburn audacity feh mplayer scrot jackd"
alias installnetworking="sudo aptitude install w3m chromium-browser"