#! /usr/bin/env python
# -*- coding: utf-8 -*-
# vim:fenc=utf-8
"""
Frame interpolation script, from https://www.reddit.com/r/osx/comments/3uttwj/mpv_and_60_fps_playback_on_os_x/

The script determines the source fps, the screen refresh rate and does the frame interpolation.
Quality is very good, but if your machine can handle it, change the pel in super to 4, hpad, vpad and blksizes to 8.
If it is slow, you can lower pel to 1, raise pads and blksizes to 32.
Also Vapoursynth doesn't work with 1080p and higher, it is too demanding on CPU.
"""

import vapoursynth as vs

core = vs.get_core()
core.std.LoadPlugin(path="/usr/local/lib/libmvtools.dylib")
core.std.LoadPlugin(path='/usr/local/lib/libffms2.dylib')

clip = video_in

src_num = int(float(container_fps) * 1e3)
src_den = int(1e3)
play_num = int(float(display_fps) * 1e3)
play_den = int(1e3)

if not (clip.width >= 1920 or clip.height >= 1080 or container_fps >= 60):
    clip = core.std.AssumeFPS(clip, fpsnum=src_num, fpsden=src_den)
    sup  = core.mv.Super(clip, pel=4, hpad=8, vpad=8)
    bvec = core.mv.Analyse(sup, truemotion=True, blksize=8, isb=True, chroma=True, search=3)
    fvec = core.mv.Analyse(sup, truemotion=True, blksize=8, isb=False, chroma=True, search=3)
    clip = core.mv.BlockFPS(clip, sup, bvec, fvec, num=play_num, den=play_den, mode=3, thscd2=48)

clip.set_output()

print("Source fps", (src_num/src_den))
print("Playback fps", (play_num/play_den))
