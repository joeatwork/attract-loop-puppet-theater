#!/bin/bash

# Usage:
#
#     ./generate-frames.prolog | ./compose-frames.sh
#
# Requires jq, imagemagick, and ffmpeg on PATH

grep -v "^#" | while read line; do
    tick=$(echo $line| jq -r '.tick')
    tickname=$(printf "%04d" "$tick")
    compositions=$(echo $line | jq -r '.sprites[] | "( sprites/\(.sheet) -filter point -resize \(.level_width)x\(.level_height) ) -gravity northwest -geometry +\(.viewport_x)+\(.viewport_y) -composite"')
    magick -size 1280x720 xc:white $compositions "renders/frame_$tickname.png"
done

# TODO: 25fps is going to end up being a pain in the ass.
ffmpeg -framerate 25 -i "renders/frame_%04d.png" renders/video.webm