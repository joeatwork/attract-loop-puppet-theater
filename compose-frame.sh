#!/bin/bash

# We get a SIZE and OFFSET in abstract "Game Units",
# and then an associated width and height in sprite pixels.

compositions=$(jq -r '"( sprites/\(.sprites[0].sheet) -filter point -resize \(.sprites[0].level_width)x\(.sprites[0].level_height) ) -gravity northwest -geometry +\(.sprites[0].level_x)+\(.sprites[0].level_y) -composite"')

magick -size 1280x720 xc:skyblue $compositions output.png