#c.tabs.position = "left"

config.load_autoconfig(True)

c.url.searchengines = {
        "DEFAULT": "https://www.google.com/search?q={}",
        "y": "https://www.youtube.com/results?hl=en&search_query={}",
        "g": "https://www.google.com/search?q={}",
        'i': 'https://duckduckgo.com/?q={}&iar=images&iax=images&ia=images',
        'd': 'https://duckduckgo.com/?q={}',
}

c.aliases = {
        "o": "open",
        "c": "open https://michlol.netanya.ac.il/Portals/student",
        "yt": "open https://youtube.com",
        "dk": "open https://start.duckduckgo.com",
        "gg": "open https://google.com",
        "mu": "open https://mahmoodsheikh.com/music",
        "gt": "open https://mahmoodsheikh.com/github",
        "pb": "open https://thepiratebay.org",
        "ru": "open https://rutracker.org",
        "r": "open https://reddit.com",
        "test": "spawn --userscript test.sh",
        "mu": "open https://4chan.org/mu",
        "t": "open trackifyapp.net/spotify/top_users",
        "q": "quit",
}

config.bind('<Ctrl-Shift-v>', 'hint links spawn --detach mpv --keep-open --force-window yes {hint-url}')
config.bind('<Ctrl-Shift-y>', 'hint links spawn --detach sh -c \'url={hint-url}; notify-send "$url"; echo "$url" | tr -d "\\n" | xclip -selection clipboard\'')
config.bind('<Ctrl-Shift-m>', 'hint links spawn --detach add_magnet.sh "{hint-url}"')

config.bind('Iv', 'hint images spawn sh -c "open_url_image.sh {hint-url}"')
config.bind('Is', 'hint images run open -t https://www.google.com/searchbyimage?&image_url={hint-url}')

#c.colors.webpage.bg = 'black'
c.colors.webpage.darkmode.enabled = True

c.content.cookies.accept = 'all'
c.content.notifications.enabled = False

c.auto_save.session = True

font_size = '15pt'
c.fonts.completion.category = font_size
c.fonts.completion.entry = font_size
c.fonts.contextmenu = font_size
c.fonts.debug_console = font_size
c.fonts.downloads = font_size
c.fonts.hints = font_size
c.fonts.keyhint = font_size
c.fonts.messages.error = font_size
c.fonts.messages.info = font_size
c.fonts.messages.warning = font_size
c.fonts.prompts = font_size
c.fonts.statusbar = font_size
# c.fonts.tabs = font_size
c.fonts.web.size.minimum = 15
c.fonts.web.size.default = 15
