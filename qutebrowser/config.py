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
}

config.bind('<Ctrl-Shift-y>', 'hint links spawn --detach mpv --force-window yes {hint-url}')

c.content.user_stylesheets = [
        # 'solarized.css',
        # 'solarized_custom.css',
        "theme.css"
]

c.colors.webpage.bg = 'black'
