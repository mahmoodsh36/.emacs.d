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

# auto completion
autoload -Uz compinit && compinit
zstyle ':completion:*' menu select
zstyle ':completion::complete:*' gain-privileges 1
# make auto completion case insensitive
zstyle ':completion:*' matcher-list '' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' '+l:|=* r:|=*'

# options
setopt AUTO_CD
setopt ALWAYS_TO_END
setopt COMPLETE_ALIASES

# prompt
export PS1="[%m@%1~] "

# load pywal colors
if [ -f ~/.cache/wal/sequences ]; then
	(cat ~/.cache/wal/sequences &)
fi

# aliases
alias ls="ls --color"
alias grep="grep --color=auto"
alias pq="pacman -Ss"
alias pi="sudo pacman -S"
alias o="xdg-open"
alias v="vim"
