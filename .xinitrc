xrdb ~/.Xresources

# Caps Lock acts as Esc when pressed by itself, and Ctrl with another key
setxkbmap -option 'ctrl:nocaps' &
xcape -e 'Control_L=Escape' &

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
