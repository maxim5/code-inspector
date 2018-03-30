#!/bin/sh
#
# ~/.xinitrc
#xcompmgr -cCfF -r7 -o.65 -l-10 -t-8 -D7 &

xset m 1 0 &
xsetroot -solid '#222222' -cursor_name left_ptr &
urxvtd -o -q -f &
mpd &
xbindkeys &
unclutter -root -idle 5 &
nitrogen --restore &
xrdb ~/.Xdefaults &

mkdir -p ~/.amazing/logs
amazing >> ~/.amazing/logs/amazing.log 2>> ~/.amazing/logs/error.log &

exec /usr/bin/awesome
#ruby /home/gig/hacks/dwm.rb | /usr/bin/dwm
#exec /usr/bin/wmii
