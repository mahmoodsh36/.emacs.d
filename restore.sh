#!/usr/bin/env sh
mkdir ~/.emacs.d/ 2>/dev/null
ln -sf "$HOME/work/emacs.d/"* "$HOME/.emacs.d/"
