#!/bin/sh
tmux has-session -t dropdown
if [ $? != 0 ]
then
    tmux new-session -s dropdown -n "main" -d

    tmux split-window -h -t dropdown:1
    tmux split-window -v -t dropdown:1.1
    #tmux split-window -h -t dropdown:1.1

    tmux send-keys -t dropdown:1.1 'clear; qalc' C-m
    tmux send-keys -t dropdown:1.0 'clear; stack ghci' C-m

    tmux new-window -n "org" -t dropdown
    tmux send-keys -t dropdown:2.0 'cd ~/Dropbox/org; nvim ~/Dropbox/org/hyrax.org' C-m

    tmux select-window -t dropdown:1
    tmux select-pane -t dropdown:1.1

fi
tmux attach -t dropdown
