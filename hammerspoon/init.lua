-- Hammerspoon config
-- See http://www.hammerspoon.org/go/
-- With parts from http://larryhynes.net/2015/04/a-minor-update-to-my-hammerspoon-config.html


-----------------------------------------------
-- Set up
-----------------------------------------------

local ctrl = {'ctrl'}
local alt = {'alt'}
local hyper = {'shift', 'cmd', 'alt', 'ctrl'}
hs.window.animationDuration = 0  -- seconds

require('hs.application')
require('hs.window')


-----------------------------------------------
-- Reload config on write (can uncomment while working on configuration)
-----------------------------------------------

-- local function reload_config(files)
--     hs.reload()
-- end
-- hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", reload_config):start()
-- hs.alert.show("Config loaded")


-----------------------------------------------
-- Hyper i to show window hints
-----------------------------------------------

hs.hotkey.bind(hyper, 'i', function()
    hs.hints.windowHints()
end)


-- -- shows a notification:
-- hs.hotkey.bind(hyper, "W", function()
--   hs.notify.new({title="Hammerspoon", informativeText="Hello World"}):send()
-- end)






-----------------------------------------------
-- Helper functions to work with window frames
-----------------------------------------------

local function modifyFrame(body)
    return function()
        if hs.window.focusedWindow() then
            local win = hs.window.focusedWindow()
            local f = win:frame()
            local screen = win:screen()
            local max = screen:frame()
            newFrame = body(win, f, screen, max)  -- In body, either modify f in-place or return a new frame
            if newFrame then
                f = newFrame
            end
            win:setFrame(f)
        else
            hs.alert.show("No active window")
        end
    end
end

local function strFrame(f)
    return '{ ' .. f.x .. ', ' .. f.y .. ', ' .. f.w .. ', ' .. f.h .. ' }'
end

local function approxEqual(x, y)
    local tolerance = 50
    return math.abs(x - y) < tolerance
end

local function approxOriginEqual(f1, f2)
    return approxEqual(f1.x, f2.x) and approxEqual(f1.y, f2.y)
end

local function approxFrameEqual(f1, f2)
    -- hs.alert.show('f1=' .. strFrame(f1) .. '          f2=' .. strFrame(f2))
    return approxEqual(f1.x, f2.x) and approxEqual(f1.y, f2.y) and
           approxEqual(f1.w, f2.w) and approxEqual(f1.h, f2.h)
end

local function leftHalf(frame)
    -- NOTE: the frame is passed as reference (i.e. the caller's object will be modified as well)!
    frame.w = frame.w / 2
    return frame
end

local function rightHalf(frame)
    -- NOTE: the frame is passed as reference (i.e. the caller's object will be modified as well)!
    frame.x = frame.x + frame.w / 2
    frame.w = frame.w / 2
    return frame
end

local function topHalf(frame)
    -- NOTE: the frame is passed as reference (i.e. the caller's object will be modified as well)!
    frame.h = frame.h / 2
    return frame
end

local function bottomHalf(frame)
    -- NOTE: the frame is passed as reference (i.e. the caller's object will be modified as well)!
    frame.y = frame.y + frame.h / 2
    frame.h = frame.h / 2
    return frame
end


--------------------------------------------------------------------
-- Cycle windows through screen halves like in Windows 7 and later
--------------------------------------------------------------------

local cycleLeft = modifyFrame(function(win, f, screen, max)
    local upperLeftCorner = approxOriginEqual(win:frame(), screen:frame())
    local isFullscreen = approxFrameEqual(win:frame(), screen:frame())

    if upperLeftCorner and (not isFullscreen) then
        -- The window touches the upper left corner, and is not maximized.
        -- Move it one screen to the left.
        local nextScreen = screen:toWest()
        -- If it's already on the leftmost screen, go all the way to the right.
        if not nextScreen then
            nextScreen = screen
            while nextScreen:toEast() do
                nextScreen = nextScreen:toEast()
            end
        end
        return rightHalf(nextScreen:frame())
    else
        -- Either fullscreen or not touching the upper left corner:
        -- Move to the left half of the same screen.
        return leftHalf(screen:frame())
    end
end)

local cycleRight = modifyFrame(function(win, f, screen, max)
    local upperMidpoint = approxOriginEqual(win:frame(), rightHalf(screen:frame()))
    local isFullscreen = approxFrameEqual(win:frame(), screen:frame())

    if approxOriginEqual(win:frame(), rightHalf(screen:frame())) then
        -- The window's left edge is near the center of the screen.
        -- Move it one screen to the right.
        local nextScreen = screen:toEast()
        -- If it's already on the rightmost screen, go all the way to the left.
        if not nextScreen then
            nextScreen = screen
            while nextScreen:toWest() do
                nextScreen = nextScreen:toWest()
            end
        end
        return leftHalf(nextScreen:frame())
    else
        -- Otherwise, move to the right half of the same screen.
        return rightHalf(screen:frame())
    end
end)

local maximize = modifyFrame(function(win, f, screen, max)
    f.x = max.x
    f.y = max.y
    f.w = max.w
    f.h = max.h
end)

hs.hotkey.bind(ctrl, 'Left', cycleLeft)
hs.hotkey.bind(ctrl, 'Right', cycleRight)
hs.hotkey.bind(ctrl, 'Up', maximize)
hs.hotkey.bind(hyper, 'h', cycleLeft)
hs.hotkey.bind(hyper, 'l', cycleRight)
hs.hotkey.bind(hyper, 'k', maximize)


-----------------------------------------------
-- Hyper+Letter to switch applications
-----------------------------------------------

local function focusApp(name)
    return function()
        if not hs.application.launchOrFocus(name) then
            hs.alert.show("Unable to focus application with name: " .. name)
        end
    end
end

hs.hotkey.bind(hyper, 'p', focusApp('Preview'));
hs.hotkey.bind(hyper, 'v', focusApp('MacVim'));
hs.hotkey.bind(hyper, 'e', focusApp('iTerm'));
hs.hotkey.bind(hyper, 't', focusApp('iTunes'));
hs.hotkey.bind(hyper, 'a', focusApp('Anki'));
hs.hotkey.bind(hyper, 'm', focusApp('Mail'));
hs.hotkey.bind(hyper, 'c', focusApp('Calendar'));
hs.hotkey.bind(hyper, 's', focusApp('Google Chrome'));
hs.hotkey.bind(hyper, 'f', focusApp('Finder'));
hs.hotkey.bind(hyper, 'j', focusApp('JEDict'));
hs.hotkey.bind(hyper, 'x', focusApp('Xcode'));
hs.hotkey.bind(hyper, 'w', focusApp('Things'));
hs.hotkey.bind(hyper, 'g', focusApp('GitHub Desktop'));


-----------------------------------------------
-- Hyper hjkl to switch window focus
-----------------------------------------------

--local function withFocusedWindow(fn)
--    return function()
--        if hs.window.focusedWindow() then
--            fn(hs.window.focusedWindow())
--        else
--            hs.alert.show("No active window")
--        end
--    end
--end

--hs.hotkey.bind(hyper, 'k', withFocusedWindow(hs.window.focusWindowNorth))
--hs.hotkey.bind(hyper, 'j', withFocusedWindow(hs.window.focusWindowSouth))
--hs.hotkey.bind(hyper, 'l', withFocusedWindow(hs.window.focusWindowEast))
--hs.hotkey.bind(hyper, 'h', withFocusedWindow(hs.window.focusWindowWest))


-----------------------------------------------
-- Ctrl/Alt+NumPad: Place window on screen
-----------------------------------------------

local function firstScreenFrame()
    return hs.screen.allScreens()[1]:frame()
end

local function secondScreenFrame()
    return hs.screen.allScreens()[2]:frame()
end

local function setWindowFrame(frame)
    return modifyFrame(function(win, f, screen, max)
        return frame
    end)
end

-- Ctrl+NumPad: Place focused window on left screen
hs.hotkey.bind(ctrl, 'pad1', setWindowFrame(bottomHalf(leftHalf(firstScreenFrame()))))
hs.hotkey.bind(ctrl, 'pad2', setWindowFrame(bottomHalf(firstScreenFrame())))
hs.hotkey.bind(ctrl, 'pad3', setWindowFrame(bottomHalf(rightHalf(firstScreenFrame()))))
hs.hotkey.bind(ctrl, 'pad4', setWindowFrame(leftHalf(firstScreenFrame())))
hs.hotkey.bind(ctrl, 'pad5', setWindowFrame(firstScreenFrame()))
hs.hotkey.bind(ctrl, 'pad6', setWindowFrame(rightHalf(firstScreenFrame())))
hs.hotkey.bind(ctrl, 'pad7', setWindowFrame(topHalf(leftHalf(firstScreenFrame()))))
hs.hotkey.bind(ctrl, 'pad8', setWindowFrame(topHalf(firstScreenFrame())))
hs.hotkey.bind(ctrl, 'pad9', setWindowFrame(topHalf(rightHalf(firstScreenFrame()))))

-- Alt+NumPad: Place focused window on right screen
hs.hotkey.bind(alt,  'pad1', setWindowFrame(bottomHalf(leftHalf(secondScreenFrame()))))
hs.hotkey.bind(alt,  'pad2', setWindowFrame(bottomHalf(secondScreenFrame())))
hs.hotkey.bind(alt,  'pad3', setWindowFrame(bottomHalf(rightHalf(secondScreenFrame()))))
hs.hotkey.bind(alt,  'pad4', setWindowFrame(leftHalf(secondScreenFrame())))
hs.hotkey.bind(alt,  'pad5', setWindowFrame(secondScreenFrame()))
hs.hotkey.bind(alt,  'pad6', setWindowFrame(rightHalf(secondScreenFrame())))
hs.hotkey.bind(alt,  'pad7', setWindowFrame(topHalf(leftHalf(secondScreenFrame()))))
hs.hotkey.bind(alt,  'pad8', setWindowFrame(topHalf(secondScreenFrame())))
hs.hotkey.bind(alt,  'pad9', setWindowFrame(topHalf(rightHalf(secondScreenFrame()))))
