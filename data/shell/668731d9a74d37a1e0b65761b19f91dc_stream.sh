#!/bin/bash

base="/var/www"

set -x

cd "$base"
rm -rf live live.h264
mkdir -p live
cd live
# ln -s "$PWD/live" "$base/live"

# fifos seem to work more reliably than pipes - and the fact that the
# fifo can be named helps ffmpeg guess the format correctly.
raspistill -w 1280 -h 720 -t 100 -vf -hf -o "$base/poster.jpg"

mkfifo live.h264
raspivid -w 1280 -h 720 -fps 25 -hf -vf -t 86400000 -b 1800000 -o - | psips > live.h264 &

# Letting the buffer fill a little seems to help ffmpeg to id the stream
sleep 2

ffmpeg -y \
  -i live.h264 \
  -f s16le -i /dev/zero -r:a 48000 -ac 2 \
  -c:v copy \
  -c:a aac -b:a 128k \
  -map 0:0 -map 1:0 \
  -f segment \
  -segment_list index.m3u8 \
  -segment_time 8 \
  -segment_format mpegts \
  -segment_list_size 80 \
  -segment_list_flags live \
  -segment_list_type m3u8 \
  -segment_wrap 10 \
  -strict -2 "%08d.ts" < /dev/null