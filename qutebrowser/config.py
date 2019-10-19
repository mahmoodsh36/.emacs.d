#c.tabs.position = "left"

c.url.searchengines = {
        "DEFAULT": "https://duckduckgo.com/?q={}",
        "y": "https://www.youtube.com/results?search_query={}",
        "g": "https://www.google.com/search?q={}",
}

c.aliases = {
        "o": "open",
        "yt": "open https://youtube.com",
        "dk": "open https://duckduckgo.com",
}

config.bind('<Ctrl-Shift-y>', 'hint links spawn --detach mpv --force-window yes {hint-url}')

c.content.user_stylesheets = [
        'solarized.css',
        'solarized_custom.css',
]
