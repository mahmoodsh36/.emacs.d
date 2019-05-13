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
ln -sf $PWD/profile ~/.profile
ln -sf $PWD/zprofile ~/.zprofile

# map caps lock to escape key
setxkbmap -option caps:swapescape
