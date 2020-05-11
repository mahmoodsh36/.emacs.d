#c.tabs.position = "left"

c.url.searchengines = {
        "DEFAULT": "https://duckduckgo.com/?q={}",
        "y": "https://www.youtube.com/results?hl=en&search_query={}",
        "g": "https://www.google.com/search?q={}",
        'i': 'https://duckduckgo.com/?q={}&iar=images&iax=images&ia=images',
        'd': 'https://duckduckgo.com/?q={}',
}

c.aliases = {
        "o": "open",
        "yt": "open https://youtube.com",
        "dk": "open https://start.duckduckgo.com",
        "gg": "open https://google.com",
        "mu": "open https://mahmoodsheikh.com/music",
        "gt": "open https://mahmoodsheikh.com/github",
        "pb": "open https://thepiratebay.org",
        "ru": "open https://rutracker.org",
        "r": "open https://reddit.com",
        "test": "spawn --userscript test.sh",
        "mu": "open https://4chan.org/mu"
        "q": "quit",
}

config.bind('<Ctrl-Shift-v>', 'hint links spawn --detach mpv --keep-open --force-window yes {hint-url}')
config.bind('<Ctrl-Shift-y>', 'hint links spawn --detach sh -c \'url={hint-url}; notify-send "$url"; echo "$url" | xclip -selection clipboard\'')

c.content.user_stylesheets = [
        # the stylesheet was taken from 'https://github.com/alphapapa/solarized-everything-css'
        'gruvbox.css',
]

c.colors.webpage.bg = 'black'

c.content.cookies.accept = 'all'
c.content.notifications = False

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
c.fonts.tabs = font_size
c.fonts.web.size.minimum = 15
c.fonts.web.size.default = 15

c.content.host_blocking.enabled = False

import sys, os
sys.path.append(os.path.join(sys.path[0], "jblock"))
config.source("jblock/jblock/integrations/qutebrowser.py")
config.set(
    "content.host_blocking.lists",
    [
        "https://easylist.to/easylist/easylist.txt",
        "https://easylist.to/easylist/easyprivacy.txt",
        "https://easylist.to/easylist/fanboy-annoyance.txt",
        "https://raw.githubusercontent.com/uBlockOrigin/uAssets/master/filters/filters.txt",
        "https://raw.githubusercontent.com/uBlockOrigin/uAssets/master/filters/annoyances.txt",
        "https://raw.githubusercontent.com/uBlockOrigin/uAssets/master/filters/badware.txt",
        "https://raw.githubusercontent.com/uBlockOrigin/uAssets/master/filters/privacy.txt",
        "https://raw.githubusercontent.com/uBlockOrigin/uAssets/master/filters/resource-abuse.txt",
        "https://raw.githubusercontent.com/uBlockOrigin/uAssets/master/filters/unbreak.txt",
        "https://www.malwaredomainlist.com/hostslist/hosts.txt",
        "https://pgl.yoyo.org/adservers/serverlist.php?hostformat=hosts&showintro=1&mimetype=plaintext",
    ],
)
