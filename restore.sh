#!/bin/sh

if [ ! -d $HOME/.config/ ]; then
    mkdir $HOME/.config/
fi

mkdir ~/.config
mkdir ~/videos
mkdir ~/downloads

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
ln -sf $PWD/user-dirs.dirs ~/.config/

# restore pacman apps
sudo pacman -R vim
sudo pacman -S $(cat pacman_programs.txt | tr "\n" " ") --noconfirm --needed

# install yay
git clone https://aur.archlinux.org/yay.git
cd yay
makepkg -si --noconfirm
cd ..
rm -rf yay

# install oh my zsh
sh -c "$(wget -O- https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
# install zsh plugins
git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting

# restore AwesomeWM config
cd ..
git clone https://github.com/mahmoodsheikh36/awesome
ln -sf $PWD/awesome/ $HOME/.config/

# restore st
git clone https://github.com/mahmoodsheikh36/st
cd st
sudo make install clean
cd ..

# restore scripts
git clone https://github.com/mahmoodsheikh36/scripts
sudo ln -sf $PWD/scripts/*.sh /usr/bin/

cd dotfiles

# restore aur apps
yay -S $(cat aur_programs.txt | tr "\n" " ") --noconfirm --needed

# install vundle
git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim

# restore wallpaper
git clone https://github.com/mahmoodsheikh36/pictures ~/pictures
