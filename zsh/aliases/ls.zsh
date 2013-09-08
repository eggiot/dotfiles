fileinfo_raw ()
{
	for f in *;
		do printf "$(expr substr $f 1 20)\`" && get_file_info $1 $f 2>/dev/null | awk '
		BEGIN {
			OFS="`"
		}
		{print $2, $1}'
	done
}

fileinfo ()
{
	fileinfo_raw $1 | column -s '`' -t -c $(tput cols)
}

# functionality additions
alias lsa="ls -lah"
alias lsd="ls -ld *(-/DN)" # list only directories and symlinks to directories
alias lsh="ls -ld .*" # list only hidden files and directories

alias lsown="fileinfo owner"

if [ "$TERM" != "dumb" ]; then
    eval "`dircolors -b`"
    alias ls='ls -F --color=auto' # colourful
    alias lsv='ls --color=auto --format=vertical' # vertical format
    alias lsl='ls --color=auto --format=long' # long format
fi