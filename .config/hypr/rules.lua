-- Window/workspace rules + misc (was rules.conf)

----------------------
---- WORKSPACE RULES ----
----------------------

hl.workspace_rule({ workspace = "w[tv1]", gaps_out = 0, gaps_in = 0 })
hl.workspace_rule({ workspace = "f[1]",   gaps_out = 0, gaps_in = 0 })
hl.workspace_rule({ workspace = "6",      gaps_out = 0, gaps_in = 0, layout = "monocle" })

-------------------
---- WINDOW RULES ----
-------------------

-- No border / rounding for single tiled windows on these workspaces
hl.window_rule({ match = { float = false, workspace = "w[tv1]" }, border_size = 0, rounding = 0 })
hl.window_rule({ match = { float = false, workspace = "f[1]" },   border_size = 0, rounding = 0 })

-- App placement / appearance
hl.window_rule({ match = { class = "brave-browser" },         workspace = "5" })
hl.window_rule({ match = { class = "emacs" },                 opacity = 0.85 })
hl.window_rule({ match = { class = "vesktop" },               workspace = "6" })
hl.window_rule({ match = { class = "org.telegram.desktop" },  workspace = "6" })
hl.window_rule({ match = { class = "Slack" },                 workspace = "6" })

hl.window_rule({ match = { class = "com.github.hluk.copyq" }, float = true })

--------------
---- MISC ----
--------------

hl.config({
    misc = {
        enable_swallow = true,
        swallow_regex  = "^(footclient|foot)$",
    },
})
