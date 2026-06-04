-- Hyprland Configuration (Lua)
-- Converted from the old hyprlang config. See https://wiki.hypr.land/Configuring/Start/
-- The old .conf files are kept as inert backups (ignored while hyprland.lua exists).

------------------
---- MONITORS ----
------------------

hl.monitor({
    output   = "",
    mode     = "highrr",
    position = "auto",
    scale    = 1.25,
})

---------------
---- INPUT ----
---------------

hl.config({
    input = {
        kb_layout  = "us,am",
        kb_variant = ",phonetic-alt",
        kb_options = "grp:alt_shift_toggle",

        follow_mouse = 1,

        sensitivity  = 0,
        repeat_rate  = 40,
        repeat_delay = 200,

        touchpad = {
            natural_scroll = true,
        },
    },
})

------------------------
---- GENERAL/LAYOUT ----
------------------------

hl.config({
    general = {
        layout = "master",
    },

    master = {
        mfact      = 0.5,
        new_on_top = true,
        new_status = "master",
    },

    xwayland = {
        force_zero_scaling = true,
    },
})

-- Gestures: workspace_swipe was commented out in the original; nothing to set here.

--------------------
---- SUB-MODULES ----
--------------------

require("exec")   -- env vars + autostart
require("style")  -- decoration, colors, animations, layer rules
require("rules")  -- window/workspace rules, misc
require("binds")  -- keybindings (+ grid-nav plugin load, currently disabled)
