-- Keybindings (was binds.conf)

local mainMod = "SUPER"

---------------------
---- BASIC BINDS ----
---------------------

hl.bind(mainMod .. " + Return",      hl.dsp.exec_cmd("foot"))
hl.bind(mainMod .. " + X",           hl.dsp.window.close())
hl.bind(mainMod .. " + CTRL + R",    hl.dsp.exec_cmd("hyprctl reload"))
hl.bind(mainMod .. " + CTRL + M",    hl.dsp.window.fullscreen({ mode = "maximized", action = "toggle" }))
hl.bind(mainMod .. " + CTRL + Space", hl.dsp.window.float({ action = "toggle" }))

----------------------------
---- APPLICATION LAUNCH ----
----------------------------

hl.bind(mainMod .. " + Space",     hl.dsp.global("quickshell:launcher"))
hl.bind(mainMod .. " + semicolon", hl.dsp.exec_cmd("rofi -show emoji"))
hl.bind(mainMod .. " + C",         hl.dsp.exec_cmd("copyq show"))
hl.bind(mainMod .. " + SHIFT + E", hl.dsp.exec_cmd("pcmanfm-qt"))
-- NOTE: SUPER+SHIFT+S is bound twice in the original (flameshot gui and grim);
-- both are preserved as-is. Resolve the duplicate if it misbehaves.
hl.bind(mainMod .. " + SHIFT + S", hl.dsp.exec_cmd("flameshot gui"))
hl.bind(mainMod .. " + SHIFT + M", hl.dsp.exec_cmd("$TERMINAL -e ncmpcpp"))
hl.bind(mainMod .. " + SHIFT + N", hl.dsp.exec_cmd("$TERMINAL -e nmtui"))
hl.bind(mainMod .. " + SHIFT + V", hl.dsp.exec_cmd("$TERMINAL -e nvim"))
hl.bind(mainMod .. " + SHIFT + B", hl.dsp.exec_cmd("$TERMINAL -e htop"))
hl.bind(mainMod .. " + SHIFT + W", hl.dsp.exec_cmd([[emacsclient -c -a ""]]))
hl.bind(mainMod .. " + SHIFT + D", hl.dsp.exec_cmd([[emacsclient -c -a "" -e '(find-file "~")']]))
hl.bind(mainMod .. " + SHIFT + S", hl.dsp.exec_cmd([[grim -g "$(slurp)" - | swappy -f -]]))
hl.bind(mainMod .. " + b",         hl.dsp.global("quickshell:toggleBar"))

-----------------------------
---- MASTER LAYOUT CTRLS ----
-----------------------------

hl.bind(mainMod .. " + J",         hl.dsp.layout("cyclenext"))
hl.bind(mainMod .. " + K",         hl.dsp.layout("cycleprev"))

hl.bind(mainMod .. " + SHIFT + J", hl.dsp.layout("swapnext"))
hl.bind(mainMod .. " + SHIFT + K", hl.dsp.layout("swapprev"))

hl.bind(mainMod .. " + SHIFT + H", hl.dsp.layout("addmaster"))
hl.bind(mainMod .. " + SHIFT + L", hl.dsp.layout("removemaster"))
hl.bind(mainMod .. " + H",         hl.dsp.layout("mfact -0.025"))
hl.bind(mainMod .. " + L",         hl.dsp.layout("mfact +0.025"))

hl.bind(mainMod .. " + CTRL + RETURN", hl.dsp.layout("swapwithmaster"))

-- Urgent window (focus urgent, else last)
hl.bind(mainMod .. " + U", hl.dsp.focus({ urgent_or_last = true }))

------------------------------------
---- WORKSPACE SWITCH (numpad) -----
------------------------------------

hl.bind(mainMod .. " + KP_End",   hl.dsp.focus({ workspace = 1 }))
hl.bind(mainMod .. " + KP_Down",  hl.dsp.focus({ workspace = 2 }))
hl.bind(mainMod .. " + KP_Next",  hl.dsp.focus({ workspace = 3 }))
hl.bind(mainMod .. " + KP_Left",  hl.dsp.focus({ workspace = 4 }))
hl.bind(mainMod .. " + KP_Begin", hl.dsp.focus({ workspace = 5 }))
hl.bind(mainMod .. " + KP_Right", hl.dsp.focus({ workspace = 6 }))
hl.bind(mainMod .. " + KP_Home",  hl.dsp.focus({ workspace = 7 }))
hl.bind(mainMod .. " + KP_Up",    hl.dsp.focus({ workspace = 8 }))
hl.bind(mainMod .. " + KP_Prior", hl.dsp.focus({ workspace = 9 }))

-- Move active window to workspace (silent = don't follow)
hl.bind(mainMod .. " + SHIFT + KP_End",   hl.dsp.window.move({ workspace = 1, follow = false }))
hl.bind(mainMod .. " + SHIFT + KP_Down",  hl.dsp.window.move({ workspace = 2, follow = false }))
hl.bind(mainMod .. " + SHIFT + KP_Next",  hl.dsp.window.move({ workspace = 3, follow = false }))
hl.bind(mainMod .. " + SHIFT + KP_Left",  hl.dsp.window.move({ workspace = 4, follow = false }))
hl.bind(mainMod .. " + SHIFT + KP_Begin", hl.dsp.window.move({ workspace = 5, follow = false }))
hl.bind(mainMod .. " + SHIFT + KP_Right", hl.dsp.window.move({ workspace = 6, follow = false }))
hl.bind(mainMod .. " + SHIFT + KP_Home",  hl.dsp.window.move({ workspace = 7, follow = false }))
hl.bind(mainMod .. " + SHIFT + KP_Up",    hl.dsp.window.move({ workspace = 8, follow = false }))
hl.bind(mainMod .. " + SHIFT + KP_Prior", hl.dsp.window.move({ workspace = 9, follow = false }))

-------------------
---- MEDIA KEYS ----
-------------------

hl.bind("XF86AudioRaiseVolume", hl.dsp.exec_cmd("wpctl set-volume @DEFAULT_AUDIO_SINK@ 2%+ -l 1"), { repeating = true })
hl.bind("XF86AudioLowerVolume", hl.dsp.exec_cmd("wpctl set-volume @DEFAULT_AUDIO_SINK@ 2%- -l 1"), { repeating = true })
hl.bind("XF86AudioMicMute",     hl.dsp.exec_cmd("wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle"))
hl.bind(mainMod .. " + XF86AudioRaiseVolume", hl.dsp.exec_cmd("mpc volume +2"))
hl.bind(mainMod .. " + XF86AudioLowerVolume", hl.dsp.exec_cmd("mpc volume -2"))

------------------
---- BRIGHTNESS ----
------------------

hl.bind("XF86MonBrightnessUp",   hl.dsp.exec_cmd("brightnessctl set +10%"))
hl.bind("XF86MonBrightnessDown", hl.dsp.exec_cmd("brightnessctl set 10%-"))

hl.bind("XF86KbdBrightnessUp",   hl.dsp.exec_cmd("echo + > /run/kbdbacklight"))
hl.bind("XF86KbdBrightnessDown", hl.dsp.exec_cmd("echo - > /run/kbdbacklight"))

------------------
---- POWER/MISC ----
------------------

hl.bind("XF86PowerOff",       hl.dsp.exec_cmd("~/.config/rofi/powermenu/type-4/powermenu.sh"))
hl.bind("XF86TouchpadToggle", hl.dsp.exec_cmd("toggletpad"))
hl.bind("XF86Launch1",        hl.dsp.exec_cmd("speedcrunch"))
hl.bind("XF86Sleep",          hl.dsp.exec_cmd("hyprlock"))

-- Waybar toggle (NOTE: shares the 'b' key with quickshell:toggleBar above)
hl.bind(mainMod .. " + B", hl.dsp.exec_cmd("killall -SIGUSR2 waybar"))

--------------------
---- MOUSE BINDS ----
--------------------

hl.bind(mainMod .. " + mouse:272", hl.dsp.window.drag(),   { mouse = true })
hl.bind(mainMod .. " + mouse:273", hl.dsp.window.resize(), { mouse = true })
hl.bind(mainMod .. " + mouse:274", hl.dsp.window.float({ action = "toggle" }))

hl.config({ binds = { scroll_event_delay = 0 } })

-----------------
---- PLUGINS ----
-----------------

-- 3x3 workspace grid navigation via mouse buttons 8/9 (hold-to-navigate).
-- Stays a C++ plugin: Hyprland's Lua API exposes no raw pointer-event hook, so
-- the hold-to-navigate behavior cannot be reproduced in pure Lua. This is the
-- direct equivalent of the old hyprlang `plugin = ...` line.
-- The .so must be built against the running Hyprland version; rebuild with
-- `make` in hypr_grid_nav/ (or `./reload.fish`) whenever Hyprland updates.
hl.plugin.load("/home/areg/.config/hypr/hypr_grid_nav/libhypr-grid-nav.so")
