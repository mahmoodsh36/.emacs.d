if test -z "$DBUS_SESSION_BUS_ADDRESS" ; then
    eval `dbus-launch --sh-syntax`
fi
# xwinwrap -ni -b -fs -ov -nf -- mplayer -cache 4096 -quiet -loop 0 -wid WID ~/ORBIT\ -\ A\ Journey\ Around\ Earth\ in\ Real\ Time\ \[\ 4k\ \]-Xjs6fnpPWy4.webm -nosound &

sxhkd &
#while true; do dwm_statusbar_update.sh; sleep 0.9; done &
#dunst &
parcellite &
pulseaudio &
# backgrounder.sh ~/pictures 200 0 & disown
nm-applet &

# songs_progress_monitor.py &
picom --config ~/.config/compton.conf &
setxkbmap -option caps:swapescape &
setxkbmap -option ctrl:ralt_rctrl
#setxkbmap -option ctrl:nocaps
feh --bg-fill ~/.cache/wallpaper &
unclutter -root &
#blueman-applet &
mpris-proxy &
#syndaemon -K -i 0.5 -R -d &

xrdb -load ~/.Xresources &

#while true; do
#  dwm 2> ~/.cache/dwm.log
#done
#

# disable beep sound
sudo rmmod pcspkr &

# start some programs
#emacs &
#google-chrome-stable &
#flatpak run com.client.Spotify || spotify &

#xsetroot -cursor_name left_ptr
exec awesome
#exec qtile
#exec bspwm
#exec emacs
