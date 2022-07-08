#!/bin/sh
#
# ~/.xinitrc
# (c) 2011-2022 by Jonas Jelten, GPLv3 or later

# executed by startx (run your window manager from here)
# in my setup, gets executed by lightdm.

if [ -d /etc/X11/xinit/xinitrc.d ]; then
    for f in /etc/X11/xinit/xinitrc.d/*; do
        [ -x "$f" ] && source "$f"
    done
fi

# this is normally run in /etc/X11/xinit/xinitrc.d/50-systemd-user.sh:
# systemctl --user import-environment DISPLAY XAUTHORITY


CHASSISFORMAT=$(hostnamectl --json=pretty | jq -r .Chassis)
export CHASSISFORMAT

[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources
[[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap

if [[ $CHASSISFORMAT == "desktop" ]]; then
    xrdb -merge <(echo "Xft.dpi: 115")
fi

echo "Parameters are: $@"
cmd=$1
if [[ $1 == "xterm" ]]; then
    cmd="$2"
fi

case $cmd in
kde)
    command="startkde"
    ;;
xfce)
    command="startxfce4"
    ;;
cinnamon)
    command="gnome-session-cinnamon"
    ;;
openbox)
    command="openbox-session"
    ;;
i3|"")
    #feh --bg-fill/scale /path/to/image &
    xsetroot -solid black
    xsetroot -bg black
    command="i3"
    ;;
*)
    shift
    command=$@
    ;;
esac

echo "command '$cmd' => run $command"
exec $command
