# exports
export TERMINAL="kitty"
export BROWSER="google-chrome-stable"
export EDITOR="nvim"

# fix a java swing problem
export _JAVA_AWT_WM_NONREPARENTING=1.
# fix plank dock problem with AwesomeWM
# export XDG_SESSION_TYPE=x11
# export XDG_RUNTIME_DIR="$HOME/.runtime"
# export XDG_CONFIG_HOME="$HOME/.config"

#source /etc/profile
export GUILE_LOAD_PATH="$HOME/workspace/guix/"
export QT_SCALE_FACTOR=2

# for pipx packages and scripts etc
export PATH="$HOME/.local/bin/:$PATH"

export PYTHONSTARTUP=$HOME/.pythonrc
export PYTHON_HISTORY_FILE=$HOME/brain/python_history