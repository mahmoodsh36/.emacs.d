#!/bin/bash

# remove unnecessary directories
rmdir ~/Desktop
rmdir ~/Documents
rmdir ~/Templates
rmdir ~/Music
rmdir ~/Public

# restore configs
ln -sf $PWD/bashrc ~/.bashrc
ln -sf $PWD/vimrc ~/.vimrc
ln -sf $PWD/Xmodmap ~/.Xmodmap
ln -sf $PWD/tmux.conf ~/.tmux.conf
ln -sf $PWD/i3.conf ~/.config/i3/config
ln -sf $PWD/i3.conf ~/.i3/config
rm -r ~/Pictures
ln -sf $PWD/Pictures ~/
mkdir -p ~/.config/terminator
ln -sf $PWD/terminator_config ~/.config/terminator/config
ln -sf $PWD/emacs ~/.emacs
ln -sf $PWD/emacs.el ~/.emacs
ln -sf $PWD/zshrc ~/.zshrc
ln -sf $PWD/inputrc ~/.inputrc
ln -sf $PWD/mimeapps.list ~/.config/mimeapps.list
ln -sf $PWD/profile ~/.profile
ln -sf $PWD/zprofile ~/.zprofile
ln -sf $PWD/xinitrc ~/.xinitrc

# map caps lock to escape key
setxkbmap -option caps:swapescape

# install vundle
git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim

# now update the system
sudo pacman -Syu --noconfirm

# install oh my zsh
# sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

sudo pacman install `cat app_list.txt | paste -sd" "`
yay -S `cat aurlist.txt | paste -sd" "` --noconfirm
