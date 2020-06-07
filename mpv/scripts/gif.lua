--[=[

gif.lua
=======

Create animated GIFs with mpv.
Requires ffmpeg.

Usage:
1. Set an A-B-Loop for the range that should be exported as GIF:
   Press "l" at the start of the range, and again at the end of the range.
2. Press "g" to export as GIF, or "G" to export as video.

Based on these similar scripts:
- https://github.com/Scheliux/mpv-gif-generator/blob/master/mpv-gif-custom-directory.lua
- https://github.com/defaultxr/giffer/blob/master/giffer


Installation
------------

-- TODO
-- Installation Instructions
-- Requires ffmpeg.
--
-- Notes on Windows:
-- * Add ffmpeg to PATH



-- TODO
--  fps=$(ffmpeg -i "$1" 2>&1 | sed -n "s/.*, \(.*\) fp.*/\1/p")
--  # filters="fps=$fps,scale=320:(320/(iw*sar))*ih:flags=lanczos" # uncomment this line and comment out the next one to force a width of 320px.
--  filters="fps=$fps,scale=iw*sar:ih:flags=lanczos"

--]=]

-- The filters to pass into ffmpeg's -vf option.
-- gif_filters="fps=24,scale=320:-1:flags=lanczos"
-- gif_filters="fps=15,scale=540:-1:flags=lanczos"
gif_filters="fps=10,scale=320:-1:flags=lanczos"
fakegif_filters="scale=1280:-1:flags=lanczos"
-- video_filters=""

local msg = require 'mp.msg'

-- Returns true iff we're on Windows
function is_windows()
    return package.config:sub(1,1) == '\\'
end

-- Returns the path to the current user's home directory
function get_home_dir()
    if is_windows() then
        return os.getenv("UserProfile")
    else
        return os.getenv("HOME")
    end
end

-- Returns the path separator of the current OS
function pathsep()
    return package.config:sub(1,1)
end

function parentpath(path)
    local sep = pathsep()
    return path:match("^(.*" .. sep .. ")")
end

function fileext(path)
    return path:match("^.*%.([^.]*)$")
end

function tmpfile()
    if is_windows() then
        return os.getenv("TEMP") .. os.tmpname()
    else
        return os.tmpname()
    end
end

function shellescape(arg)
    -- TODO improve
    return '"' .. string.gsub(arg, '"', '"\\""') .. '"'
end

function file_exists(path)
    local f = io.open(path, "r")
    if f ~= nil then
        io.close(f)
        return true
    else
        return false
    end
end

function get_new_filename(base, ext)
    local filename = string.format("%s.%s", base, ext)
    if not file_exists(filename) then
        return filename
    else
        for i = 2, 999 do
            local filename = string.format("%s (%d).%s", base, i, ext)
            if not file_exists(filename) then
                return filename
            end
        end
    end
end

function export_loop(convert_to_gif)
    --msg.fatal(mp.get_property("path"))
    --msg.fatal(mp.get_property("stream-path"))
    --msg.fatal(mp.get_property("filename"))
    --msg.fatal(mp.get_property("input-file"))
    --local output_dir = os.getenv("HOME") .. pathsep() .. "Downloads" .. pathsep()
    --msg.fatal(output_dir)
    --msg.fatal(parentpath(mp.get_property("path")))
    --msg.fatal(fileext(mp.get_property("path")))
    --if true then return end

    local t1 = mp.get_property("ab-loop-a")
    local t2 = mp.get_property("ab-loop-b")

    if t1 == "no" or t2 == "no" then
        mp.osd_message("Error: export_loop: no A-B-Loop set")
        return
    end

    local d = t2 - t1

    local input_file = mp.get_property("path", "")

    local output_ext
    if convert_to_gif then
        output_ext = "mp4"
        -- output_ext = "gif"
    else
        output_ext = fileext(input_file)
    end

    local output_dir = get_home_dir() .. pathsep() .. "Downloads" .. pathsep()
    local output_file = get_new_filename(output_dir .. mp.get_property("filename/no-ext") .. " " .. t1, output_ext)
    if output_file == nil then
        mp.osd_message("Error: export_loop: found no available filename")
        return
    end

    if convert_to_gif then
        export_fakegif(t1, d, input_file, output_file)
    else
        export_video(t1, d, input_file, output_file)
    end
end

-- NOTE: we don't really want *.gif format as it's very inefficient.
-- This command actually creates a "highly compatible *.mp4" video which should serve the same purpose.
function export_fakegif(position, duration, input_file, output_file)
    mp.osd_message(string.format("Exporting as mp4-FakeGIF (%s, duration %s)...", position, duration))

    -- Note: Using -an to remove audio
    -- Maybe check out this command line:
    --      ffmpeg -i <input> -c:v libx264 -crf 23 -profile:v baseline -level 3.0 -movflags faststart -pix_fmt yuv420p -vf "scale=trunc(iw/2)*2:trunc(ih/2)*2" -an <output.mp4>
    -- from https://www.reddit.com/r/Telegram/comments/948hs7/q_how_do_i_get_telegram_to_render_a_clip_as_gif
    cmd = string.format("ffmpeg -v warning -ss %s -i %s -t %s -vf %s -an -y %s", position, shellescape(input_file), duration, shellescape(fakegif_filters), shellescape(output_file))
    msg.debug(cmd)
    os.execute(cmd)

    msg.info("Exported to " .. output_file)
    mp.osd_message("Exported to " .. output_file)
end

function export_gif(position, duration, input_file, output_file)
    mp.osd_message(string.format("Exporting as GIF (%s, duration %s)...", position, duration))

    local palette_file = tmpfile() .. ".png"

    -- Create palette
    cmd = string.format("ffmpeg -v warning -ss %s -t %s -i %s -vf %s -y %s", position, duration, shellescape(input_file), shellescape(gif_filters .. ",palettegen"), shellescape(palette_file))
    msg.debug(cmd)
    os.execute(cmd)
    -- TODO: Use mp.command (with "subprocess"?) instead of os.execute. Should be able to save the manual escaping
    -- TODO: Extract command-running into a function

    -- Create actual GIF
    cmd = string.format("ffmpeg -v warning -ss %s -t %s -i %s -i %s -lavfi %s -y %s", position, duration, shellescape(input_file), shellescape(palette_file), shellescape(gif_filters .. " [x]; [x][1:v] paletteuse"), shellescape(output_file))
    msg.debug(cmd)
    os.execute(cmd)
    os.remove(palette_file)

    msg.info("Created GIF at " .. output_file)
    mp.osd_message("Created GIF at " .. output_file)
end

function export_video(position, duration, input_file, output_file)
    mp.osd_message(string.format("Exporting (%s, duration %s)...", position, duration))

    -- cmd = string.format("ffmpeg -v warning -ss %s -i %s -t %s -vf %s -an -y %s", position, shellescape(input_file), duration, shellescape(video_filters), shellescape(output_file))
    cmd = string.format("ffmpeg -v warning -ss %s -i %s -t %s -y %s", position, shellescape(input_file), duration, shellescape(output_file))
    msg.debug(cmd)
    os.execute(cmd)

    msg.info("Exported to " .. output_file)
    mp.osd_message("Exported to " .. output_file)
end

mp.add_key_binding("g", "export_loop(true)", function() export_loop(true) end)
mp.add_key_binding("G", "export_loop(false)", function() export_loop(false) end)
