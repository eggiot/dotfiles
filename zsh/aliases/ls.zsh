# functionality additions
alias lsd="ls -ld *(-/DN)" # list only directories and symlinks to directories
alias lsh="ls -ld .*" # list only hidden files and directories

if [ "$TERM" != "dumb" ]; then
    eval "`dircolors -b`"
    alias ls='ls -F --color=auto' # colourful
    alias lsv='ls --color=auto --format=vertical' # vertical format
    alias lsl='ls --color=auto --format=long' # long format
fi