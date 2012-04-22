#!/bin/sh
#must be run as root
tar -xvzf Dina-PCF.tar.gz
rm /etc/fonts/conf.d/70-no-bitmaps.conf
fc-cache -f -v
cp -r Dina-PCF /usr/share/fonts/dina
mkfontdir /usr/share/fonts/dina
xset fp rehash
fc-cache -f
fc-list | grep Dina
rm -rf Dina-PCF