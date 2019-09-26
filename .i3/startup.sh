#!/bin/sh

xset r rate 200 60
~/.xmonad/arandr_laptop_ext.sh
feh --bg-scale /home/andre/andre/bg/gray-smooth-seamless-background.jpg
xautolock -time 5 -locker "gnome-screensaver-command -l" -notify 10 -notifier "notify-send -t 5000 -i gtk-dialog-info 'Locking in 10 seconds'" &

# Network Applet
if [ -z "$(pgrep nm-applet)" ] ; then
    nm-applet &
fi

if [ -z "$(pgrep keynav)" ] ; then
    keynav &
fi
