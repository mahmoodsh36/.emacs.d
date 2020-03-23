if test -z "$DBUS_SESSION_BUS_ADDRESS" ; then
    eval `dbus-launch --sh-syntax`
fi
# xwinwrap -ni -b -fs -ov -nf -- mplayer -cache 4096 -quiet -loop 0 -wid WID ~/ORBIT\ -\ A\ Journey\ Around\ Earth\ in\ Real\ Time\ \[\ 4k\ \]-Xjs6fnpPWy4.webm -nosound &

sxhkd &
while true; do dwm_statusbar_update.sh; sleep 0.9; done &
dwm_statusbar.sh &
dunst &
pasystray &
parcellite &
#pulseaudio &
# backgrounder.sh ~/pictures 200 0 & disown

# songs_progress_monitor.py &
compton --config ~/.config/compton.conf &
setxkbmap -option caps:swapescape &
hsetroot -fill ~/.cache/wallpaper
unclutter -root &
# blueman-applet &
syndaemon -K -i 0.5 -R -d &

expressvpn connect &

[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources

while true; do
    dwm 2> ~/.cache/dwm.log
done

#exec awesome
