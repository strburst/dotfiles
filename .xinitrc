xrdb ~/.Xresources

# Caps Lock acts as Esc when pressed by itself, and Ctrl with another key
setxkbmap -option 'ctrl:nocaps' &
xcape -e '#66=Escape' &

dropbox &
numlockx &
redshift &

sxhkd -f 100 &
exec bspwm
