#!/bin/bash

# restore configs
ln -sf $PWD/bashrc ~/.bashrc
ln -sf $PWD/vimrc ~/.vimrc
ln -sf $PWD/Xmodmap ~/.Xmodmap
ln -sf $PWD/tmux.conf ~/.tmux.conf
ln -sf $PWD/i3.conf ~/.config/i3/config
ln -sf $PWD/i3.conf ~/.i3/config
ln -sf $PWD/Pictures ~/
ln -sf $PWD/terminator_config ~/.config/terminator/config
ln -sf $PWD/emacs ~/.emacs
ln -sf $PWD/emacs.el ~/.emacs
ln -sf $PWD/zshrc ~/.zshrc
ln -sf $PWD/inputrc ~/.inputrc
ln -sf $PWD/mimeapps.list ~/.config/mimeapps.list

# restore setup
dconf load /org/gnome/terminal/legacy/profiles:/ < gnome_terminal_profiles.dconf 

# restore apps

 sudo xdg-settings set default-web-browser opera.desktop
