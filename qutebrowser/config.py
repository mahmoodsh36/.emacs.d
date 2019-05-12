
# play youtube videos in mpv
# config.bind('V', 'hint links spawn --detach mpv --ytdl-format=best --force-window yes {hint-url}')
config.bind('V', "hint links spawn --detach youtube-dl -f best -o - '{hint-url}' | mpv -")
c.content.user_stylesheets = [
    'solarized-dark.css',
    'custom_solarized.css'
]
