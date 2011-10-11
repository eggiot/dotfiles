########################################################
# .zshrc                                               #
# by Eliot Walker                                      #
# 11 Oct 2011                                          #
#                                                      #
# requires aptitude most sox scrot git feh w3m mplayer #
########################################################

###########
# OPTIONS #
###########

# load zsh modules
autoload -U compinit promptinit zcalc zsh-mime-setup
compinit
promptinit
zsh-mime-setup

# options
setopt printexitvalue # alert me if something's failed
setopt GLOB_COMPLETE
setopt PUSHD_MINUS
setopt RM_STAR_WAIT
setopt ZLE
setopt NO_BEEP
setopt NO_CASE_GLOB
setopt NUMERIC_GLOB_SORT
setopt EXTENDED_GLOB
setopt IGNORE_EOF
setopt MENUCOMPLETE
setopt ALL_EXPORT

###########
# HISTORY #
###########

HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000

setopt EXTENDED_HISTORY # puts timestamp in history
setopt HIST_VERIFY # when using ! cmds, confirm first
setopt HIST_IGNORE_DUPS # ignore same command run twice
setopt APPEND_HISTORY # don't overwrite history
setopt SHARE_HISTORY # _all_ zsh sessions share the same history
setopt INC_APPEND_HISTORY # write after each command

# search history
autoload -U history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey '^[[A' history-beginning-search-backward-end
bindkey '^[[B' history-beginning-search-forward-end


setopt autocd extendedglob nomatch
unsetopt appendhistory beep
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/eliot/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

# PROMPT
PROMPT='%n@%M:%~$ ' # default prompt

##############
# MY ALIASES #
##############
# package management
alias pkg="aptitude"
alias spkg="sudo aptitude"
alias spkgi="sudo aptitude -P install"
alias spkgu="sudo aptitude update && sudo aptitude -P safe-upgrade"
alias spkgr="sudo aptitude -P remove"
alias spkgc="sudo aptitude clean"
alias pkgs="aptitude search"
alias pkgsh="aptitude show"
alias pkgd="aptitude download"
alias pkgsf="apt-cache-formatted"

## directory navigation
alias desktop="~/Desktop"
alias "..."="../.."
alias "...."="../../.."
alias "....."="../../../../"
alias 1tb="/media/1TB"

# shortened
alias fp="ps -aux | grep" # find a running process
alias turnoff="sudo shutdown -h now" # shut down the computer
alias alsa-lmms="pasuspender -- lmms" # start lmms using alsa
alias ff="ls -a | grep" # find file
alias set-menu-button-layout="gconftool-2 --set /apps/metacity/general/button_layout --type string"

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
alias emacs="emacs -nw"

# media
alias pinknoise="play -n synth pinknoise"
alias brownnoise="play -n synth brownnoise"
alias whitenoise="play -n synth whitenoise"
alias scrot="scrot -q 100"

## git
alias git-init="git init-db"
alias git-commit="git commit -a"
alias git-add-all="git add -A"

# suffix aliases
alias -s png=feh
alias -s jpg=feh
alias -s jpeg=feh
alias -s html=w3m
alias -s org=w3m
alias -s avi=mplayer

# networking
alias ping="ping -c 3"

## complicated alias functions

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

##############
# Completion #
##############

setopt CORRECT			# command CORRECTION

zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache

# Fuzzy matching of completions for when you mistype them
zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*:match:*' original only
zstyle ':completion:*:approximate:*' max-errors 1 numeric

# Prevent CVS files/directories from being completed
zstyle ':completion:*:(all-|)files' ignored-patterns '(|*/)CVS'
zstyle ':completion:*:cd:*' ignored-patterns '(*/)#CVS'

# allow one error for every three characters typed in approximate completer
zstyle -e ':completion:*:approximate:*' max-errors \
    'reply=( $(( ($#PREFIX+$#SUFFIX)/2 )) numeric )'

alias mv='nocorrect mv'       # no spelling correction on mv
alias cp='nocorrect cp'
alias mkdir='nocorrect mkdir'

# ignore completion functions (until the _ignored completer)
zstyle ':completion:*:functions' ignored-patterns '_*'
zstyle ':completion:*:*:*:users' ignored-patterns \
        adm apache bin daemon games gdm halt ident junkbust lp mail mailnull \
        named news nfsnobody nobody nscd ntp operator pcap postgres radvd \
        rpc rpcuser rpm shutdown squid sshd sync uucp vcsa xfs avahi-autoipd\
        avahi backup messagebus beagleindex debian-tor dhcp dnsmasq fetchmail\
        firebird gnats haldaemon hplip irc klog list man cupsys postfix\
        proxy syslog www-data mldonkey sys snort

## add colors to processes for kill completion
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'

zstyle ':completion:*:*:kill:*:processes' command 'ps --forest -A -o pid,user,cmd'
zstyle ':completion:*:processes-names' command 'ps axho command'

zstyle ':completion:*:*:killall:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:*:killall:*:processes' command 'ps --forest -A -o pid,user,cmd'

# match uppercase from lowercase
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}'


zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-prompt '%SAt %p: Hit TAB for more, or the character to insert%s'
zstyle ':completion:*' menu select=1 _complete _ignored _approximate

zstyle ':completion:*' select-prompt '%SScrolling active: current selection at %p%s'

# Completion Styles

# list of completers to use
#zstyle ':completion:*::::' completer _expand _complete _ignored _approximate

# insert all expansions for expand completer
zstyle ':completion:*:expand:*' tag-order all-expansions

# formatting and messages
zstyle ':completion:*' verbose yes
zstyle ':completion:*:descriptions' format '%B%d%b'
zstyle ':completion:*:messages' format '%d'
zstyle ':completion:*:warnings' format 'No matches for: %d'
zstyle ':completion:*:corrections' format '%B%d (errors: %e)%b'
zstyle ':completion:*' group-name ''

# offer indexes before parameters in subscripts
zstyle ':completion:*:*:-subscript-:*' tag-order indexes parameters