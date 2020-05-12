from libqtile.config import Key, Screen, Group, Drag, Click
from libqtile.command import lazy
from libqtile import layout, bar, widget

sup = "mod4"
alt = "mod1"

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
    Key([sup, "control"], "j", lazy.layout.grow_down()),
    Key([sup, "control"], "k", lazy.layout.grow_up()),
    Key([sup, "control"], "h", lazy.layout.grow_left()),
    Key([sup, "control"], "l", lazy.layout.grow_right()),
    Key([sup, "shift"], "n", lazy.layout.normalize()),
    Key([sup], "t", lazy.layout.toggle_split()),
]

groups = [
     Group('code'),
     Group('music'),
     Group('web'),
     Group('term'),
     Group('etc'),
     Group('etc'),
]

for index, grp in enumerate(groups):

     # index is the position in the group list grp is the group object.
     # We assign each group object a set of keys based on it's
     # position in the list.

     # Eventually we will implement a function to change the name based
     # on what window is active in that group.

     keys.extend([

             # switch to group
         Key([sup], str(index+1), lazy.group[grp.name].toscreen()),

             # send to group
         Key([sup, "shift"], str(index+1), lazy.window.togroup(grp.name)),
    ])

layouts = [
    layout.Bsp(),
    layout.Floating(),
]

# orange text on grey background
default_data = dict(fontsize=17,
                    foreground="FF6600",
                    background="1D1D1D",
                    font="Source Code Pro")

screens = [
     Screen(bottom = bar.Bar([
          widget.GroupBox(**default_data),
          widget.WindowName(**default_data),
          widget.TextBox(text='hey there', **default_data),
          widget.Sep(),
          widget.TextBox(text='🔊', **default_data),
          widget.Volume(**default_data),
          widget.Sep(),
          widget.Clock(**default_data),
          widget.Systray(**default_data),
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

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
