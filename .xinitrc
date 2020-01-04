if test -z "$DBUS_SESSION_BUS_ADDRESS" ; then
    eval `dbus-launch --sh-syntax`
fi
# xwinwrap -ni -b -fs -ov -nf -- mplayer -cache 4096 -quiet -loop 0 -wid WID ~/ORBIT\ -\ A\ Journey\ Around\ Earth\ in\ Real\ Time\ \[\ 4k\ \]-Xjs6fnpPWy4.webm -nosound &

sxhkd &
dwm_statusbar.sh &
nm-applet &
dunst &
pasystray &
parcellite &
pulseaudio &
# backgrounder.sh ~/pictures 200 0 & disown

# songs_progress_monitor.py &
compton --config ~/.config/compton.conf &
setxkbmap -option caps:swapescape &
hsetroot -full ~/.cache/wallpaper
unclutter &
# unclutter -root &
# blueman-applet &
syndaemon -K -i 0.5 -R -d &

while true; do
    dwm 2> ~/.cache/dwm.log
done
