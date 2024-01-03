#!/usr/bin/env sh

mkdir ~/.emacs.d/
ln -sf "$HOME/work/dotfiles/.emacs.d/"* "$HOME/.emacs.d/"

ln -sf "$HOME/work/dotfiles/.gitconfig" "$HOME/"

for filename in .xinitrc .zshrc .zprofile .Xresources .vimrc .tmux.conf; do
	ln -sf "$HOME/work/dotfiles/$filename" "$HOME/"
done

mkdir "$HOME/.config/"
for filename in alacritty compton.conf gtk-3.0 mimeapps.list mpv vifm qutebrowser kitty\
	rofi sxhkd sxiv user-dirs.dirs transmission-daemon zathura; do
	ln -sf "$HOME/work/dotfiles/.config/$filename" "$HOME/.config/"
done

ln -sf "$HOME/work/dotfiles/.config/transmission-daemon" "$HOME/.config/transmission"

mkdir "$HOME/.config/guix/"
ln -sf "$HOME/work/dotfiles/channels.scm" "$HOME/.config/guix/"
sudo ln -sf "$HOME/work/dotfiles/config.scm" "/etc/config.scm"

mkdir "$HOME/.config/nvim/"
ln -sf "$HOME/work/dotfiles/.vimrc" "$HOME/.config/nvim/init.vim"
