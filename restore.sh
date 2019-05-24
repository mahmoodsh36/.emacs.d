#!/bin/bash
# i have to run this as root

USER="mahmooz"

# remove unnecessary directories
# rmdir /home/$USER/Desktop
# rmdir /home/$USER/Documents
# rmdir /home/$USER/Templates
# rmdir /home/$USER/Music
# rmdir /home/$USER/Public

echo "user is $USER"
# restore configs
ln -sf $PWD/bashrc              /home/$USER/.bashrc
ln -sf $PWD/bashrc              /home/$USER/.bash_profile
ln -sf $PWD/vimrc               /home/$USER/.vimrc
ln -sf $PWD/Xmodmap             /home/$USER/.Xmodmap
ln -sf $PWD/tmux.conf           /home/$USER/.tmux.conf
ln -sf $PWD/i3.conf             /home/$USER/.config/i3/config
ln -sf $PWD/i3.conf             /home/$USER/.i3/config
rm -r                           /home/$USER/Pictures
ln -sf $PWD/Pictures            /home/$USER/
mkdir -p                        /home/$USER/.config/terminator
ln -sf $PWD/terminator_config   /home/$USER/.config/terminator/config
mkdir /home/$USER/.emacs.d/
ln -sf $PWD/config.org          /home/$USER/.emacs.d/config.org
ln -sf $PWD/emacs.el            /home/$USER/.emacs
ln -sf $PWD/zshrc               /home/$USER/.zshrc
ln -sf $PWD/inputrc             /home/$USER/.inputrc
ln -sf $PWD/mimeapps.list       /home/$USER/.config/mimeapps.list
ln -sf $PWD/profile             /home/$USER/.profile
ln -sf $PWD/zprofile            /home/$USER/.zprofile
ln -sf $PWD/xinitrc             /home/$USER/.xinitrc
ln -sf $PWD/i3status            /home/$USER/.config/i3status
ln -sf $PWD/i3/                 /home/$USER/.config/i3
ln -sf $PWD/fonts               /home/$USER/.fonts
ln -sf $PWD/Xresources          /home/$USER/.Xresources

# install vundle
git clone https://github.com/VundleVim/Vundle.vim.git /home/$USER/.vim/bundle/Vundle.vim

# now update the system
sudo pacman -Syu --noconfirm

sudo pacman -R vim --noconfirm
sudo pacman -S `cat app_list.txt | paste -sd" "` --noconfirm

# install yay
git clone https://aur.archlinux.org/yay.git
cd yay
makepkg -si

# switch to user because yay doesnt work with root permissions
# sudo --user $USER yay -S `cat aur_list.txt | paste -sd" "` --noconfirm

# install oh my zsh
# sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

