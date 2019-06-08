# colors
export RED=$'\e[1;31m'
export GRN=$'\e[1;32m'
export YEL=$'\e[1;33m'
export BLU=$'\e[1;34m'
export MAG=$'\e[1;35m'
export CYN=$'\e[1;36m'
export END=$'\e[0m'

export MYSCRIPTS=~/codee/scripts/

set -o vi
export EDITOR="vim"
export VISUAL="vim"
export TERMINAL="terminator"
export BROWSER="chromium"

# those variables helped fix tmux with airline
LANG="en_US.UTF-8"
LC_COLLATE="en_US.UTF-8"
LC_CTYPE="en_US.UTF-8"
LC_MESSAGES="en_US.UTF-8"
LC_MONETARY="en_US.UTF-8"
LC_NUMERIC="en_US.UTF-8"
LC_TIME="en_US.UTF-8"
LC_ALL="en_US.UTF-8"

alias ls="ls --color"
alias l="ls --color=none"
alias grep="grep --color"
alias try_wallpaper="find *jpg *png -exec feh --bg-fill {} \; -exec echo {} \; -exec sleep 3 \;"

function c () {
  cd "$@" && ls
  # pwd | tr -d "\n" > ~/.last_dir
}

# PS1 prompt customizing
function getDirName () {
  dir=`pwd`
  if [ "$dir" == "/" ]; then
    echo $dir
  elif [ "$dir" == "~" ]; then
    echo "$dir"
  elif [ "$dir" == "${HOME}" ]; then
    echo "~"
  else
    echo $dir | rev | cut -d "/" -f 1 | rev
  fi
}
if [[ $EUID -ne 0 ]]; then
    export PS1="\[\033[0;34m\] [\$(getDirName)]\[\033[0m\] "
else
    export PS1="\[\033[0;34m\] (\$(getDirName))\[\033[0m\] "
fi

setxkbmap -option caps:swapescape

cd ~

# fix intellij having white blank screen
export _JAVA_AWT_WM_NONREPARENTING=1

# finally, start tmux
if [[ ! $TERM =~ screen ]]; then
    exec tmux
fi

