compton --config ~/.config/compton.conf &

while [ true ]
do
  total_mem=`free -h | head -2 | tail -1 | awk '{ print $2 }'`
  used_mem=`free -h | head -2 | tail -1 | awk '{ print $3 }'`
  date=" `date "+%y/%m/%d %H:%M"`"
  volume=" $(amixer get Master | tail -1 | cut -d"[" -f2 | cut -d"]" -f1)"
  mem=" $used_mem / $total_mem"


  # disk
  root_partition=`lsblk -r | grep " /$" | cut -d" " -f1`
  root_space=`df "/dev/$root_partition" -h | tail -1 | awk '{ print $2 }'`
  used_root_space=`df "/dev/$root_partition" -h | tail -1 | awk '{ print $3 }'`
  space=" $used_root_space / $root_space"

  # spotify
  artist=`dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.freedesktop.DBus.Properties.Get string:'org.mpris.MediaPlayer2.Player' string:'Metadata'|egrep -A 2 "artist"|egrep -v "artist"|egrep -v "array"|cut -b 27-|cut -d '"' -f 1|egrep -v ^$` || echo "spotify hasnt been launched"
  title=`dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.freedesktop.DBus.Properties.Get string:'org.mpris.MediaPlayer2.Player' string:'Metadata'|egrep -A 1 "title"|egrep -v "title"|cut -b 44-|cut -d '"' -f 1|egrep -v ^$` || echo "spotify hasnt been launched"
  spotify=" $title - $artist"

  # network
  net_interface=`nmcli -f DEVICE d | grep -v "DEVICE" | head -1`
  net_ssid=`nmcli connection show | grep $net_interface | cut -d" " -f1`
  net=" $net_ssid"

  xsetroot -name "  $spotify  |  $mem  |  $space  |  $volume |  $net  |  $date  "

  sleep 0.5
done &

feh --bg-fill ~/pictures/purple_flowers.jpg &
/home/mahmooz/workspace/spotify_tracker/spotify_tracker.py /home/mahmooz/mydata/track_data.json &
xss-lock -- i3lock -i ~/pictures/wallpaper.png &
setxkbmap -option caps:swapescape

exec dwm & sleep 100d
