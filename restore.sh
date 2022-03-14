#!/usr/bin/env sh

mkdir ~/.emacs.d/
ln -s "$HOME/workspace/dotfiles/.emacs.d/init.el" "$HOME/.emacs.d/"
ln -s "$HOME/workspace/dotfiles/.emacs.d/snippets" "$HOME/.emacs.d/"

for filename in .xinitrc .zshrc .zprofile .Xresources .vimrc .tmux.conf; do
	ln -s "$HOME/workspace/dotfiles/$filename" "$HOME/"
done

mkdir "$HOME/.config/"
for filename in alacritty compton.conf gtk-3.0 mimeapps.list mpv vifm qutebrowser kitty\
	rofi sxhkd sxiv user-dirs.dirs transmission-daemon zathura; do
	ln -s "$HOME/workspace/dotfiles/.config/$filename" "$HOME/.config/"
done

mkdir "$HOME/.config/guix/"
ln -s "$HOME/workspace/dotfiles/channels.scm" "$HOME/.config/guix/"
sudo ln -sf "$HOME/workspace/dotfiles/config.scm" "/etc/config.scm"

mkdir "$HOME/.config/nvim/"
ln -s "$HOME/workspace/dotfiles/.vimrc" "$HOME/.config/nvim/init.vim"
