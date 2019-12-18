function video_downloader()
    vidurl = mp.get_property("path")
    os.execute('notify-send "downloading video" && youtube-dl -o "/home/$USER/media/vid/toffee/%(title)s-%(id)s.%(ext)s" "' .. vidurl .. '" 2>&1 > /home/$USER/.cache/mpv_dl_log && notify-send "finished downloading video" &')
end
function save_local_video()
    vid_path = mp.get_property("path")
    os.execute("mv '" .. vid_path .. "' $HOME/media/vid/toffee/")
    mp.command('stop')
end
function remove_video()
    vid_path = mp.get_property("path")
    os.execute('mv $(printf "%q" $(echo ' .. vid_path .. '))" $HOME/.local/share/Trash/files')
    mp.command('stop')
end
mp.add_key_binding("d", "download_video", video_downloader)
mp.add_key_binding("s", "save_local_video", save_local_video)
mp.add_key_binding("x", "remove_video", remove_video)
