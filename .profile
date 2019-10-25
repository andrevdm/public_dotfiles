# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi


#CUSTOM

~/bin/caps
~/bin/bg.sh
#setxkbmap us

#! dropbox start
#dropbox-cli start

#xbacklight = 100
# unity-settings-daemon &
# gnome-screensaver &

#xscreensaver -no-splash & 

export EnableNuGetPackageRestore=true
export GOPATH=/home/andre/prog/go
export PATH="${PATH}:/home/andre/.rbenv/bin:$PATH"
export PATH="${PATH}:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games"
export PATH="${PATH}:/usr/local/go/bin:/home/andre/bin:/home/andre/.cabal/bin"
export PATH="${PATH}:/home/andre/.local/bin:/usr/local/go/bin"
export PATH="${PATH}:/home/andre/bin"
export PATH="${PATH}:${GOPATH}/bin"
export EDITOR=vim
export VISUAL=vim
export NDK_HOME=/media/andre/data/android-sdk/ndk-bundle
export ANDROID_HOME=/opt/android-sdk/

export GIT_SSL_CAINFO=/etc/ssl/certs/ca-certificates.crt
export SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt

# alias grep="/usr/bin/grep $GREP_OPTIONS"
unset GREP_OPTIONS

#keyboard repeat rate:  xset r rate [delay] [rate]
xset r rate 200 60 #25

alias open=xdg-open
if [ -e /home/andre/.nix-profile/etc/profile.d/nix.sh ]; then . /home/andre/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
