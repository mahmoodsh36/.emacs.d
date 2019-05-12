# this is all my config
# if [ -d ~/codee/scripts ] ; then
#   export PATH="$PATH:~/codee/scripts"
# fi

# colors
export RED=$'\e[1;31m'
export GRN=$'\e[1;32m'
export YEL=$'\e[1;33m'
export BLU=$'\e[1;34m'
export MAG=$'\e[1;35m'
export CYN=$'\e[1;36m'
export END=$'\e[0m'

export MYSCRIPTS=~/codee/scripts/

# enable vi mode in terminal
set -o vi
# enable vim with tmux
export TERM="xterm-256color"
# make vim default editor
export EDITOR="vim"
export VISUAL="vim"
export TERMINAL="gnome-terminal"
export BROWSER="chromium"

# aliases
alias ls="ls --color"
alias grep="grep --color"

# my logo
/home/mahmood/codee/scripts/logo.sh

# those variables helped fix tmux with airline
LANG="en_US.UTF-8"
LC_COLLATE="en_US.UTF-8"
LC_CTYPE="en_US.UTF-8"
LC_MESSAGES="en_US.UTF-8"
LC_MONETARY="en_US.UTF-8"
LC_NUMERIC="en_US.UTF-8"
LC_TIME="en_US.UTF-8"
LC_ALL="en_US.UTF-8"
