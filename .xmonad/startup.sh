#!/bin/sh

mkfifo /tmp/xmonad.pipe.log
mkfifo /tmp/xmonad.pipe.base
echo " " /tmp/xmonad.pipe.base

xset r rate 200 60
~/.xmonad/arandr_laptop_ext.sh
feh --bg-scale /home/andre/andre/bg/gray-smooth-seamless-background.jpg
xautolock -time 5 -locker "gnome-screensaver-command -l" -notify 10 -notifier "notify-send -t 5000 -i gtk-dialog-info 'Locking in 10 seconds'" &

# stalonetray
if [ -z "$(pgrep stalonetray)" ] ; then
    stalonetray &
fi

# Network Applet
if [ -z "$(pgrep nm-applet)" ] ; then
    nm-applet &
fi

if [ -z "$(pgrep keynav)" ] ; then
    keynav &
fi


# # System tray
# # Power manager
# if [ -z "$(pgrep xfce4-power-manager)" ] ; then
#     xfce4-power-manager &
# fi

# # Redshift
# if [ -z "$(pgrep redshift)" ] ; then
#     redshift &
# fi

# # Wallpaper
# if [ -z "$(pgrep nitrogen)" ] ; then
#     nitrogen --restore &
# fi

# # Screensaver
# if [ -z "$(pgrep xscreensaver)" ] ; then
#     xscreensaver -no-splash &
# fi

# # compton
# if [ -z "$(pgrep compton)" ] ; then
#     compton -b &
# fi

# xbindkeys
# xbindkeys
