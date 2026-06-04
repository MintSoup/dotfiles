-- Environment variables + autostart (was exec.conf)

-------------------------------
---- ENVIRONMENT VARIABLES ----
-------------------------------

-- env = XCURSOR_SIZE,24
hl.env("TERMINAL", "foot")
hl.env("QSG_RENDER_LOOP", "threaded")
hl.env("QT_QPA_PLATFORMTHEME", "qt6ct")
hl.env("ELECTRON_OZONE_PLATFORM_HINT", "auto")

-------------------
---- AUTOSTART ----
-------------------

hl.on("hyprland.start", function()
    hl.exec_cmd("brave")

    hl.exec_cmd("vesktop",  { workspace = "6 silent" })
    hl.exec_cmd("slack",    { workspace = "6 silent" })
    hl.exec_cmd("Telegram", { workspace = "6 silent" })

    hl.exec_cmd("emacs --daemon")
    -- hl.exec_cmd("flameshot")
    hl.exec_cmd("copyq")
    hl.exec_cmd("qs")
    hl.exec_cmd("hyprpaper")
end)
