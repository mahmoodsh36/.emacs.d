#!/usr/bin/env sh

for filename in .xinitrc .zshrc .zprofile .Xresources .vimrc .emacs .tmux.conf; do
	ln -s "$HOME/workspace/dotfiles/$filename" "$HOME/"
done

mkdir "$HOME/.config/"
for filename in alacritty compton.conf gtk-3.0 mimeapps.list mpv ranger\
	rofi sxhkd sxiv user-dirs.dirs transmission-daemon; do
	ln -s "$HOME/workspace/dotfiles/.config/$filename" "$PWD/.config/"
done

mkdir "$HOME/.config/guix/"
ln -s "$HOME/workspace/dotfiles/channels.scm" "$HOME/.config/guix/"
sudo ln -sf "$HOME/workspace/dotfiles/config.scm" "/etc/config.scm"
