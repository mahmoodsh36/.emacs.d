export QT_QPA_PLATFORMTHEME="qt5ct"
export EDITOR=/usr/bin/emacsclient
export GTK2_RC_FILES="$HOME/.gtkrc-2.0"
# fix "xdg-open fork-bomb" export your preferred browser from here
export BROWSER=/usr/bin/chromium

# start exwm when we login on tty5
[ -z "$DISPLAY" -a "$(tty)" = '/dev/tty5' ] && exec xinit -- vt05
