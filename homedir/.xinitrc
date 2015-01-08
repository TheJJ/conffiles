#!/bin/sh
#
# ~/.xinitrc
# (c) 2011-2015 by Jonas Jelten, GPLv3 or later

# executed by startx (run your window manager from here)
# in my setup, gets executed by slim.

if [ -d /etc/X11/xinit/xinitrc.d ]; then
	for f in /etc/X11/xinit/xinitrc.d/*; do
		[ -x "$f" ] && source "$f"
	done
fi

[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources
[[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap

# disable bell
xset -b

# faster key repetition and shorter delay
# default is 600ms, 25Hz
xset r rate 180 45

echo "Parameters are: $@"
cmd=$1
if [[ $1 == "xterm" ]]; then
	cmd="$2"
fi

case $cmd in
kde)
	COMMAND="startkde"
	;;
xfce)
	COMMAND="startxfce4"
	;;
cinnamon)
	COMMAND="gnome-session-cinnamon"
	;;
openbox)
	COMMAND="openbox-session"
	;;
i3|"")
	#feh --bg-fill/scale /path/to/image &
	xsetroot -solid black
	xsetroot -bg black
	COMMAND="i3"
	;;
*)
	shift
	COMMAND=$@
	;;
esac

echo "command '$cmd' => run $COMMAND"
exec $COMMAND
