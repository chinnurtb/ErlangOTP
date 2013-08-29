#!/bin/sh
## start my local demons

for i in /home/joe/services/*.sh 
  do
    /bin/su joe $i start
  done
