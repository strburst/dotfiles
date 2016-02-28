xrdb ~/.Xresources

# Initialize gnome-keyring
eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
export SSH_AUTH_SOCK

# Caps Lock acts as Esc when pressed by itself, and Ctrl with another key
setxkbmap -option 'ctrl:nocaps' &
xcape -e 'Control_L=Escape' &

# Place the wallpaper
feh --bg-center ~/Pictures/wallpaper.png

# Start startup applications
dropbox &
dunst &
nm-applet &
numlockx &
redshift &
rescuetime &
stalonetray &

sxhkd -f 100 &
exec bspwm
