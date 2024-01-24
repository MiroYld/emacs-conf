#!/usr/bin/env bash

config_file="$HOME/.config/polybar/grayblocks/config.ini"

function start_panel() {
    # Terminate already running bar instances
    killall -q polybar

    # Wait until the processes have been shut down
    while
	pgrep -u $UID -x polybar >/dev/null;
    do
	sleep 1;
    done
    polybar -q main -c "$config_file" &	
}
start_panel
