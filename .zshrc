# cursor handling for vi-mode
function zle-keymap-select zle-line-init zle-line-finish {
  case $KEYMAP in
    vicmd)         echo -ne '\e[1 q';;
    viins|main)    echo -ne '\e[5 q';;
  esac

  zle reset-prompt
  zle -R
}
zle -N zle-line-init
zle -N zle-line-finish
zle -N zle-keymap-select

# vim keys
bindkey -v
export KEYTIMEOUT=1

# bindings
bindkey '^P' up-history
bindkey '^N' down-history
bindkey '^r' history-incremental-search-backward
bindkey '^f' history-incremental-search-forward
autoload edit-command-line; zle -N edit-command-line
bindkey -M vicmd v edit-command-line

# auto completion
autoload -Uz compinit && compinit
zstyle ':completion:*' menu select
zstyle ':completion::complete:*' gain-privileges 1
# make auto completion case insensitive
zstyle ':completion:*' matcher-list '' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' '+l:|=* r:|=*'

# enable completion for aliases
_complete_alias() {
    [[ -n $PREFIX ]] && compadd -- ${(M)${(k)galiases}:#$PREFIX*}
    return 1
}
zstyle ':completion:*' completer _complete_alias _complete _ignored

# options
setopt AUTO_CD
setopt ALWAYS_TO_END
setopt COMPLETE_ALIASES
# setopt SHARE_HISTORY
setopt HIST_IGNORE_DUPS
setopt RM_STARSILENT
setopt EXTENDED_HISTORY
setopt INC_APPEND_HISTORY_TIME
setopt HIST_FIND_NO_DUPS

# prompt
# export PS1="[%m@%1~]$ "
export PS1="[%1~]$ "
export PYTHONSTARTUP=$HOME/.pythonrc
export PYTHON_HISTORY_FILE=$HOME/.python_history

# aliases
alias ls="ls --color"
alias grep="grep --color=auto"
alias o="open.sh"
alias v="vim"
alias pi="sudo pacman -S --noconfirm"
alias pq="pacman -Ss"
alias history='history 1'
alias l="ls"
alias youtube-mp3='youtube-dl --ignore-errors --extract-audio --audio-format mp3'
alias gs="git status"
alias gc="git commit -a -m"
alias gp="git push"
alias p="pwd"
alias m="mpv --keep-open"
alias vi="echo viewing images && find . -type f -exec file --mime {} \; | grep 'image/' | cut -d ':' -f1 | xargs -d '\n' sxiv -a"
alias vv="echo viewing videos && find . -type f -exec file --mime {} \; | grep 'video/' | cut -d ':' -f1 | xargs -d'\n' -n1 open.sh"
alias tor_new_ip="echo -e 'AUTHENTICATE ""\r\nsignal NEWNYM\r\nQUIT' | nc 127.0.0.1 9051"
alias vol="pactl list sinks | awk '/^\s*Volume/{print \$5}'"
# nanoseconds since epoch
alias nse="date +%s.%N"
alias dl="curl -O"
alias pg="ping mahmoodsheikh.com"
alias xi="sudo xbps-install -y"
alias xq="xbps-query -Rs"
alias fm="ffmpeg -i"
alias t="mimetype"
alias vj="vim -c 'set syntax=json' -"
alias aas="add_album_song.sh"

# open image by name from folder ~/media/images/toffee
oi() {
    find ~/media/images/toffee -name "$1" -exec open.sh {} \;
}
# find files with a certain mimetype
ffwm() {
    mime="$1"
    find -type f | parallel -j+1 mimetype | grep --color=no "$mime" | rev | cut -d ':' -f2- | rev
}
# view images in reverse order
vir() {
    find -type f | grep -o 'e[0-9]\+\(_\|$\)' | tr -d 'e' | tr -d '_' | sort -nr | while read number; do echo num: $number; find . \( -name "file${number}_*" -or -name "image${number}_*" \) -exec open.sh {} \;; done
}
# view images in ascending order
vio() {
    echo viewing images && find . -type f -exec mimetype {} \; | grep 'image/' | cut -d ':' -f1 | xargs -d '\n' sxiv -a
}
# view images randomly
viR() {
    find -type f -exec mimetype {} \; | grep 'image/' | rev | cut -d ':' -f2- | rev | shuf | while read image; do echo viewing $image; open.sh "$image"; done
}
# open all files in current directory using open.sh script
oa() {
    trap "exit" 2
    for file in $(ls --color=no); do
        echo $file 
        open.sh $file || return;
    done
}
# get the difference in percentage between 2 images
cmp_image() {
    convert "$1" "$2" -compose Difference -composite \
        -colorspace gray -format '%[fx:mean*100]' info:
}
# cd and ls into directory
c() {
    cd $@; ls
}
# do some math
math() { awk "BEGIN {print ${@:1}}"; }

# command history
HISTSIZE=1000000
SAVEHIST=1000000
HISTFILE=~/media/zsh_history

IFS='
'
