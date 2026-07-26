-- Decoration, colors, animations, layer rules (was style.conf)

-----------------------
---- LOOK AND FEEL ----
-----------------------

hl.config({
    general = {
        gaps_in     = 1.5,
        gaps_out    = 3,
        border_size = 2,
        col = {
            active_border   = { colors = { "rgba(51afefee)", "rgba(c678ddee)" }, angle = 45 },
            inactive_border = "rgba(00000000)",
        },
    },

    decoration = {
        rounding = 0,

        blur = {
            enabled = true,
            size    = 6,
            passes  = 2,
            noise   = 0,
            popups_ignorealpha = 0.2
        },

        shadow = {
            range        = 300,
            render_power = 4,
            color        = "rgba(1a1a1aaf)",
            offset       = "0 40",
            scale        = 0.2,
        },
    },

    animations = {
        enabled = true,
    },
})

-------------------
---- ANIMATIONS ----
-------------------

hl.animation({ leaf = "windows",    enabled = true, speed = 1,    bezier = "default" })
hl.animation({ leaf = "windowsOut", enabled = true, speed = 0.75, bezier = "default" })

hl.animation({ leaf = "border",     enabled = true, speed = 2,    bezier = "default" })

hl.animation({ leaf = "fade",       enabled = true, speed = 1,    bezier = "default" })
hl.animation({ leaf = "fadeOut",    enabled = true, speed = 1,    bezier = "default" })

hl.animation({ leaf = "workspaces", enabled = true, speed = 1.75, bezier = "default", style = "slide" })

-------------------
---- LAYER RULES ----
-------------------

hl.layer_rule({ match = { namespace = "waybar" },     blur = true })
hl.layer_rule({ match = { namespace = "rofi" },       blur = true })
hl.layer_rule({ match = { namespace = "quickshell" }, blur = true })
hl.layer_rule({ match = { namespace = "quickshell" }, ignore_alpha = 0.01 })
hl.layer_rule({ match = { namespace = "quickshell" }, no_anim = true })

-- Emoji picker: separate namespace so its blur cuts off higher up the fade
-- (matching the perceived content) instead of lingering as an empty blurred
-- rect. It stays permanently mapped, so no_anim isn't strictly needed but is
-- kept for parity.
hl.layer_rule({ match = { namespace = "emoji-picker" }, blur = true })
hl.layer_rule({ match = { namespace = "emoji-picker" }, no_anim = true })
hl.layer_rule({ match = { namespace = "emoji-picker" }, ignore_alpha = 0.2 })

-- App launcher: same treatment as the emoji picker -- permanently mapped, so
-- its blur needs a higher ignore_alpha to fade out cleanly with the dim.
hl.layer_rule({ match = { namespace = "app-launcher" }, blur = true })
hl.layer_rule({ match = { namespace = "app-launcher" }, no_anim = true })
hl.layer_rule({ match = { namespace = "app-launcher" }, ignore_alpha = 0.2 })
