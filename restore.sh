#!/bin/sh

if [ ! -d $HOME/.config/ ]; then
    mkdir $HOME/.config/
fi

# restore config files
ln -sf $PWD/.vimrc $HOME/
ln -sf $PWD/.xinitrc $HOME/
ln -sf $PWD/.emacs $HOME/
ln -sf $PWD/.inputrc $HOME/
ln -sf $PWD/.tmux.conf $HOME/
ln -sf $PWD/.zshrc $HOME/
ln -sf $PWD/compton.conf $HOME/.config/
ln -sf $PWD/mimeapps.list $HOME/.config/
ln -sf $PWD/rofi $HOME/.config/
ln -sf $PWD/vifm $HOME/.config/

# restore pacman apps


# restore aur apps


# restore github apps
