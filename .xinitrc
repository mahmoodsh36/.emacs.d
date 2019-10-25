compton --config ~/.config/compton.conf &

setxkbmap -option caps:swapescape &

backgrounder.sh ~/pictures 200 &
dropbox &
nm-applet &
dunst &
sxhkd > ~/.cache/sxhkd_log &

# test for an existing bus daemon, just to be safe
if test -z "$DBUS_SESSION_BUS_ADDRESS" ; then
  # if not found, launch a new one
  eval `dbus-launch --sh-syntax --exit-with-session`
  echo "D-Bus per-session daemon address is: $DBUS_SESSION_BUS_ADDRESS"
fi

exec qtile > ~/.cache/qtile_log
