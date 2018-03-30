#!/bin/sh

sudo /sbin/modprobe snd-bcm2835
sudo java -jar /home/osmc/limelight/limelight.jar pair
sudo pkill xboxdrv
sudo /usr/bin/xboxdrv --config /home/osmc/limelight/padconfig.ini --dpad-rotation 90 --axismap -DPAD_X=DPAD_X --trigger-as-button --silent & java -jar /home/osmc/limelight/limelight.jar stream -1080 -60fps -bitrate 30000
