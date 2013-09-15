# create a playlist / create sorted list of files
make-sorted-file-list () {
    find -maxdepth 1 -type f -iname \*$1 | sort > $2
}

alias pinknoise="play -n synth pinknoise"
alias brownnoise="play -n synth brownnoise"
alias whitenoise="play -n synth whitenoise"
alias scrot="scrot -q 100"
alias groovesalad="mplayer -playlist http://somafm.com/groovesalad.pls"

svg2png () {
	inkscape --file="$1" --export-png="$2" --export-width=$3
}