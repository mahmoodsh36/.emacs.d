# disabled vi mode in zsh for now, i use emacs vterm
# cursor handling for vi-mode
#function zle-keymap-select zle-line-init zle-line-finish {
  #case $KEYMAP in
    #vicmd)         echo -ne '\e[1 q';;
    #viins|main)    echo -ne '\e[5 q';;
  #esac

  #zle reset-prompt
  #zle -R
#}
#zle -N zle-line-init
#zle -N zle-line-finish
#zle -N zle-keymap-select

# vim keys
#bindkey -v
#export KEYTIMEOUT=1
bindkey -e

# bindings
#bindkey '^P' up-history
#bindkey '^N' down-history
#bindkey '^r' history-incremental-search-backward
#bindkey '^f' history-incremental-search-forward
autoload edit-command-line; zle -N edit-command-line
#bindkey -M vicmd v edit-command-line

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
#setopt SHARE_HISTORY
setopt HIST_IGNORE_DUPS
setopt RM_STARSILENT
setopt EXTENDED_HISTORY
setopt INC_APPEND_HISTORY_TIME
setopt HIST_FIND_NO_DUPS
setopt interactivecomments

# prompt
if [ -n "$SSH_CLIENT" ]; then
  ip_addr=$(ip addr | grep 'inet\s' | grep -v '127.0.0.1' | tr -s ' ' | cut -d ' ' -f3 | cut -d'/' -f1 | head -1)
  export PS1="$ip_addr %1~ λ "
elif [[ ! -z "$IN_NIX_SHELL" ]]; then
  export PS1=$(echo '\033[0;32m[NIX]\033[0m%1~ λ ')
else
  export PS1="%1~ λ "
fi

# aliases
alias l="ls"
alias ls="ls --color"
alias grep="grep --color=auto"
alias o="xdg-open"
alias vim="nvim"
alias v="vim"
alias pi="sudo pacman -S --noconfirm"
alias pq="pacman -Ss"
alias history='history 1'
alias ytadl='youtube-dl -f bestaudio --extract-audio --add-metadata'
alias gc="git commit -a -m"
alias s="git status"
alias gp="git push"
alias p="pwd"
alias m="mpv --keep-open"
alias tor_new_ip="echo -e 'AUTHENTICATE ""\r\nsignal NEWNYM\r\nQUIT' | nc 127.0.0.1 9051"
alias vol="pactl list sinks | awk '/^\s*Volume/{print \$5}'"
# nanoseconds since epoch
alias nse="date +%s.%N"
alias dl="curl -O"
alias pg="ping google.com"
alias xi="sudo xbps-install -y"
alias xq="xbps-query -Rs"
alias fm="ffmpeg -i"
alias t="mimetype"
alias vj="vim -c 'set syntax=json' -"
alias ti='date +%s%3N'
alias locate='locate -i'
alias of='o $(fzf)'
alias spc="view_audio_spectrum.sh"
#alias rsync="ionice -c2 -n7 rsync -t"
alias ion="ionice -c2 -n7"
alias calc="bc -l"
alias bde="bg; disown; exit"
alias psg="ps -e | grep -i"
alias mt="file --mime-type -b"
alias cp="rsync -a --times --info=progress2"
alias fr="adb reverse tcp:5000 tcp:5000; flutter run"
alias ytdl='yt-dlp'
alias nrs="sudo nixos-rebuild switch"
alias ncu="sudo nix-channel --update"
alias tra="transmission-remote"
alias duh="du -hs"
alias se="sudoedit"
alias nix-shell="nix-shell --command zsh"
alias dla="yt-dlp -f bestaudio --extract-audio --add-metadata --split-chapters --embed-thumbnail" # short for download album

# cd and ls into directory
c() {
    cd $@; ls
}

# do some math
math() { awk "BEGIN {print ${@:1}}"; }

# colors for man pages
export LESS_TERMCAP_mb=$'\e[1;32m'
export LESS_TERMCAP_md=$'\e[1;32m'
export LESS_TERMCAP_me=$'\e[0m'
export LESS_TERMCAP_se=$'\e[0m'
export LESS_TERMCAP_so=$'\e[01;33m'
export LESS_TERMCAP_ue=$'\e[0m'
export LESS_TERMCAP_us=$'\e[1;4;31m'

# command history
HISTSIZE=1000000
SAVEHIST=1000000
HISTFILE=~/brain/zsh_history

IFS='
'

# plugin management
setup_plugins() {
    [ ! -d ~/.config/zsh ] && mkdir ~/.config/zsh
    cd ~/.config/zsh
    git clone https://github.com/zsh-users/zsh-syntax-highlighting.git
    git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
}
load_plugins() {
    source ~/.config/zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
}
load_plugins 2>/dev/null

setup_yay() {
    sudo pacman -S --needed git base-devel
    git clone https://aur.archlinux.org/yay.git
    cd yay
    makepkg -si
    cd ..
    rm -rf yay
}

# ssh issues with kitty fix
export TERM=xterm-256color
export EDITOR=nvim

# for macos
if [ -x /usr/libexec/path_helper ]; then
    eval `/usr/libexec/path_helper -s`
fi

# stop zsh from creating ~/.zcompdump (by changing the location)
export ZSH_COMPDUMP=$ZSH/cache/.zcompdump-$HOST