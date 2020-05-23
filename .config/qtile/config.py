from libqtile.config import Key, Screen, Group, Drag, Click
from libqtile.command import lazy
from libqtile import layout, bar, widget
import subprocess

sup = "mod4"
alt = "mod1"

default_theme = dict(fontsize=17,
                     foreground="#cccccc",
                     background="1D1D1D",
                     font="Source Code Pro")

def resize(qtile, direction):
    layout = qtile.current_layout
    child = layout.current
    parent = child.parent

    while parent:
        if child in parent.children:
            layout_all = False

            if (direction == "left" and parent.split_horizontal) or (
                direction == "up" and not parent.split_horizontal
            ):
                parent.split_ratio = max(5, parent.split_ratio - layout.grow_amount)
                layout_all = True
            elif (direction == "right" and parent.split_horizontal) or (
                direction == "down" and not parent.split_horizontal
            ):
                parent.split_ratio = min(95, parent.split_ratio + layout.grow_amount)
                layout_all = True

            if layout_all:
                layout.group.layout_all()
                break

        child = parent
        parent = child.parent

@lazy.function
def resize_left(qtile):
    resize(qtile, "left")


@lazy.function
def resize_right(qtile):
    resize(qtile, "right")


@lazy.function
def resize_up(qtile):
    resize(qtile, "up")


@lazy.function
def resize_down(qtile):
    resize(qtile, "down")

keys = [
    # Switch window focus to other pane(s) of stack
    Key([sup], "space", lazy.layout.next()),

    # Swap panes of split stack
    Key([sup, "shift"], "space", lazy.layout.rotate()),

    # Toggle between split and unsplit sides of stack.
    # Split = all windows displayed
    # Unsplit = 1 window displayed, like Max layout, but still with
    # multiple stack panes
    Key([sup, "shift"], "Return", lazy.layout.toggle_split()),

    # Toggle between different layouts as defined below
    Key([sup], "Tab", lazy.next_layout()),
    Key([sup], "q", lazy.window.kill()),

    Key([sup, "control"], "r", lazy.restart()),
    Key([sup, "control"], "q", lazy.shutdown()),

    Key([sup, "shift"], "space", lazy.window.toggle_floating()),

    Key([sup], "j", lazy.layout.down()),
    Key([sup], "k", lazy.layout.up()),
    Key([sup], "h", lazy.layout.left()),
    Key([sup], "l", lazy.layout.right()),
    Key([sup, "shift"], "j", lazy.layout.shuffle_down()),
    Key([sup, "shift"], "k", lazy.layout.shuffle_up()),
    Key([sup, "shift"], "h", lazy.layout.shuffle_left()),
    Key([sup, "shift"], "l", lazy.layout.shuffle_right()),
    Key([sup, alt], "j", lazy.layout.flip_down()),
    Key([sup, alt], "k", lazy.layout.flip_up()),
    Key([sup, alt], "h", lazy.layout.flip_left()),
    Key([sup, alt], "l", lazy.layout.flip_right()),
    Key([sup, "control"], "j", resize_down),
    Key([sup, "control"], "k", resize_up),
    Key([sup, "control"], "h", resize_left),
    Key([sup, "control"], "l", resize_right),
    Key([sup, "shift"], "n", lazy.layout.normalize()),
    Key([sup], "t", lazy.layout.toggle_split()),

]

groups = [Group(str(group_num)) for group_num in range(1, 7)]

for index, grp in enumerate(groups):
    keys.extend([
        Key([sup], str(index+1), lazy.group[grp.name].toscreen()),
        Key([sup, "shift"], str(index+1), lazy.window.togroup(grp.name)),
    ])

layouts = [
    layout.Bsp(),
    layout.Floating(),
]

def current_song():
    return subprocess.check_output('current_spotify_song.sh').decode().replace('\n', '')

def keyboard_layout():
    cmd = "setxkbmap -query | awk '/layout/ {print $2}'"
    return subprocess.check_output(cmd, shell=True).decode().replace('\n', '')

screens = [
     Screen(top = bar.Bar([
          widget.GroupBox(**default_theme),
          widget.WindowName(**default_theme),
          widget.GenPollText(func=keyboard_layout, update_interval=0.7, **default_theme),
          widget.Sep(),
          widget.TextBox(text='🎧', **default_theme),
          widget.GenPollText(func=current_song, update_interval=0.3, **default_theme),
          widget.Sep(),
          widget.TextBox(text='🔊', **default_theme),
          widget.Volume(**default_theme),
          widget.Sep(),
          widget.TextBox(text='🕗', **default_theme),
          widget.Clock(**default_theme),
          widget.Systray(**default_theme),
     ], 27))
]

# Drag floating layouts.
mouse = [
    Drag([sup], "Button1", lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([sup], "Button3", lazy.window.set_size_floating(),
         start=lazy.window.get_size()),
    Click([sup], "Button2", lazy.window.bring_to_front())
]

dgroups_key_binder = None
dgroups_app_rules = []
main = None
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
floating_layout = layout.Floating(float_rules=[
    {'wmclass': 'confirm'},
    {'wmclass': 'dialog'},
    {'wmclass': 'download'},
    {'wmclass': 'error'},
    {'wmclass': 'file_progress'},
    {'wmclass': 'notification'},
    {'wmclass': 'splash'},
    {'wmclass': 'toolbar'},
    {'wmclass': 'confirmreset'},  # gitk
    {'wmclass': 'makebranch'},  # gitk
    {'wmclass': 'maketag'},  # gitk
    {'wname': 'branchdialog'},  # gitk
    {'wname': 'pinentry'},  # GPG key password entry
    {'wmclass': 'ssh-askpass'},  # ssh-askpass
])
auto_fullscreen = True
focus_on_window_activation = "smart"

wmname = "LG3D"
