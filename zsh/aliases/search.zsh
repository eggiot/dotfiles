# find a running process
fp() {
    ps ax -o pid,command | grep $1 | sed -e "/[0-9] grep $1/d" -e "/[0-9] sed /d" | sed G | changecolour 2 $1
}

# find file
alias ff="ls -a | grep"

# recursive find file
rff() {
	find -ls | grep $1 | strip_long_ls
}

alias cff="locate"

# search a man page for a pattern
manfind() {
    man $1 | grep $2
}