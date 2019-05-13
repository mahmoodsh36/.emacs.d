#!/bin/bash
# switch to user because yay doesnt work with root permissions
USER="mahmooz"
su $USER
yay -S `cat aur_list.txt | paste -sd" "` --noconfirm
