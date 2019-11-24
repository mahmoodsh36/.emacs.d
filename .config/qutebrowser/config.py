#c.tabs.position = "left"

c.url.searchengines = {
        "DEFAULT": "https://duckduckgo.com/?q={}",
        "y": "https://www.youtube.com/results?search_query={}",
        "g": "https://www.google.com/search?q={}",
        'i': 'https://duckduckgo.com/?q={}&iar=images&iax=images&ia=images',
        'd': 'https://duckduckgo.com/?q={}',
}

c.aliases = {
        "o": "open",
        "yt": "open https://youtube.com",
        "dk": "open https://duckduckgo.com",
        "gg": "open https://google.com",
        "mu": "open https://mahmoodsheikh.com/music",
        "gt": "open https://mahmoodsheikh.com/github",
}

config.bind('<Ctrl-Shift-y>', 'hint links spawn --detach mpv --force-window yes {hint-url}')

c.content.user_stylesheets = [
        "style.css"
]

c.colors.webpage.bg = 'black'
