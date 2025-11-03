#!/usr/bin/env bash
mkdir ~/.emacs.d/ 2>/dev/null
ln -sf "$WORK_DIR/emacs.d/"* "$HOME/.emacs.d/"
