function video_downloader()
    vidurl = mp.get_property("path")
    os.execute('notify-send "downloading video" && youtube-dl -o "/home/$USER/media/vid/toffee/%(title)s-%(id)s.%(ext)s" "' .. vidurl .. '" 2>&1 > /home/$USER/.cache/mpv_dl_log && notify-send "finished downloading video" &')
end
mp.add_key_binding("d", "download_video", video_downloader)
