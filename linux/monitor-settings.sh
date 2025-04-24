#!/bin/sh

# Вариант с мелким монитором
# xrandr --output VGA-1 --mode 1280x1024 --pos 1920x0 --rotate normal --output HDMI-1 --primary --mode 1920x1080 --pos 0x0 --rotate normal

# Вариант со вторым большим монтором
xrandr \
    --output VGA-1  --mode 1920x1080 --pos 0x0    --rotate normal \
    --output HDMI-1 --mode 1920x1080 --pos 0x1080 --rotate normal --primary
