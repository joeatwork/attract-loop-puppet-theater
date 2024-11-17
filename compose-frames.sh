#!/bin/bash

# Requires jq, imagemagick, and ffmpeg

while read line; do
    tick=$(echo $line| jq -r '.tick')
    tickname=$(printf "%04d" "$tick")
    compositions=$(echo $line | jq -r '.sprites[] | "( sprites/\(.sheet) -filter point -resize \(.level_width)x\(.level_height) ) -gravity northwest -geometry +\(.level_x)+\(.level_y) -composite"')
    magick -size 1280x720 xc:skyblue $compositions "renders/frame_$tickname.png"
done

# TODO: This frame rate is going to be an issue
# TODO: This should probably be 60 FPS
ffmpeg -framerate 25 -i "renders/frame_%04d.png" renders/video.webm