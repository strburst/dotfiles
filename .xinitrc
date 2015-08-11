xrdb ~/.Xresources

# Make Java Swing applications spawn windows properly
wmname LG3D

setxkbmap -option 'ctrl:nocaps' &
xcape -e '#66=Escape' &

dropbox &
numlockx &
redshift &

sxhkd -f 100 &
exec bspwm
