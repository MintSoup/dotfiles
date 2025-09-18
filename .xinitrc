#!/bin/sh

export PATH=$PATH:~/.local/bin

export TERMINAL=st
export GDK_SCALE=1.25
export GDK_DPI_SCALE=1.25
export EDITOR=nvim
export QT_QPA_PLATFORMTHEME=qt6ct
export QT_STYLE_OVERIDE=qt6ct
export DEBUGINFOD_URLS="https://debuginfod.archlinux.org/"
# export QT_SCALE_FACTOR=1.25


userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# dwm
exec awesome
# exec emacs
tail -f /dev/null
# cd ~/Tools/mintywm
# exec ./mintywm
