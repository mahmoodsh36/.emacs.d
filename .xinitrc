# test for an existing bus daemon, just to be safe
if test -z "$DBUS_SESSION_BUS_ADDRESS" ; then
  # if not found, launch a new one
  eval `dbus-launch --sh-syntax --exit-with-session`
  echo "D-Bus per-session daemon address is: $DBUS_SESSION_BUS_ADDRESS"
fi &

# xwinwrap -ni -b -fs -ov -nf -- mplayer -cache 4096 -quiet -loop 0 -wid WID ~/videos/water.mp4 &

sxhkd &
dunst &
dropbox &
nm-applet &
# backgrounder.sh ~/pictures 200 0 & disown
dwm_statusbar.sh &
songs_progress_monitor.py &
compton --config ~/.config/compton.conf &
setxkbmap -option caps:swapescape &
hsetroot -fill ~/pictures/cafe.jpg &

while true; do
    dwm 2> ~/.cache/dwm
done
