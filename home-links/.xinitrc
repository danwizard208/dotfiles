#!/bin/zsh

#Set screenshot directory
export SCREENSHOT_DIR="$HOME/Dropbox/Photos/Screenshots"

# Load resources
xrdb -merge $HOME/.Xresources

#Set cursor
xsetroot -cursor_name redglass

# Enable compositing
if [[ -x $(which xcompmgr) ]] ; then
    xcompmgr -c &
fi

#Have the cursor disappear when it isn't useful (two seconds idleness)
if [[ -x $(which unclutter) ]] ; then
    unclutter -root -idle 2 &
fi

# Set up an icon tray
trayer --edge top --align right --SetDockType true --SetPartialStrut true \
 --expand true --width 10 --transparent true --tint 0x000000 --height 17 --margin 0 &

# Set the background
if [[ -x ~/.fehbg ]] ; then
    ~/.fehbg
fi

Fire up apps
if [[ -x $(which xscreensaver) ]] ; then
    xscreensaver -no-splash &
fi
if [[ -x $(which xfce4-volumed) ]] ; then
    xfce4-volumed
fi
if [[ -x $(which dropbox) ]] ; then
    dropbox start &
fi

# Perform any key remappings
if [[ -r "$HOME/.xmodmaprc" ]] ; then
    xmodmap "$HOME/.xmodmaprc"
fi

# Set up path
# if [[ -r "$HOME/.path_before" ]] ; then
#     eval BEFORE=`paste -s -d: $HOME/.path_before`
#     eval AFTER=`paste -s -d: $HOME/.path_after`
#     export PATH="$BEFORE:$PATH:$AFTER"
# fi

#Launch xmonad

# exec xmonad
ssh-agent xmonad
