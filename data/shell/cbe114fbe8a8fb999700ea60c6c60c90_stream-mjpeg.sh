#!/bin/bash
echo "Starting pi cam stream (mjpg)"
# disable HDMI, use p to enable
/opt/vc/bin/tvservice -o 
cd /usr/src/mjpg-streamer-pi
LD_LIBRARY_PATH=./ ./mjpg_streamer -o "output_http.so -w ./www -c user:pass" -i "input_raspicam.so -d 100 -x 960 -y 720 -d 100"
