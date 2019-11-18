if test -z "$DBUS_SESSION_BUS_ADDRESS" ; then
    eval `dbus-launch --sh-syntax`
fi
# xwinwrap -ni -b -fs -ov -nf -- mplayer -cache 4096 -quiet -loop 0 -wid WID ~/videos/water.mp4 &

sxhkd &
dunst_pywal.sh &
dropbox &
nm-applet &
pasystray &
# backgrounder.sh ~/pictures 200 0 & disown
dwm_statusbar.sh &
songs_progress_monitor.py &
compton --config ~/.config/compton.conf &
setxkbmap -option caps:swapescape &
hsetroot -fill ~/.cache/wallpaper &
unclutter &
blueman-applet &

# while true; do
#     dwm 2> ~/.cache/dwm
# done
exec awesome
