local awful = require("awful")
local gears = require("gears")

terminal = os.getenv("TERMINAL")

return gears.table.join(
    awful.key({modkey}, "Return", function () awful.spawn(terminal) end,
			  {description = "ST", group = "launcher"}),
    awful.key({modkey}, "space", function () awful.spawn.with_shell("~/.config/rofi/launchers/type-3/launcher.sh") end,
			  {description = "Rofi", group = "launcher"}),
    awful.key({modkey}, ";", function () awful.spawn("rofi -show emoji") end,
			  {description = "Insert emojis", group = "launcher"}),
    awful.key({modkey}, "c", function () awful.spawn("copyq show") end,
			  {description = "Run CopyQ", group = "launcher"}),

    awful.key({modkey, "Shift"}, "e", function () awful.spawn("pcmanfm-qt") end,
			  {description = "PCManFM", group = "launcher"}),
    awful.key({modkey, "Shift"}, "s", function () awful.spawn("flameshot gui", false) end,
			  {description = "Flameshot", group = "launcher"}),
    awful.key({modkey, "Shift"}, "m", function () awful.spawn(terminal .. " -e ncmpcpp") end,
			  {description = "ncmpcpp", group = "launcher"}),
    awful.key({modkey, "Shift"}, "n", function () awful.spawn(terminal .. " -e sh -c \"nmtui\" ") end,
			  {description = "PCManFM", group = "launcher"}),
    awful.key({modkey, "Shift"}, "v", function () awful.spawn(terminal .. " -e nvim") end,
			  {description = "Neovim", group = "launcher"}),
    awful.key({modkey, "Shift"}, "b", function () awful.spawn(terminal .. " -e htop") end,
			  {description = "Htop", group = "launcher"}),
    awful.key({modkey, "Shift"}, "w", function () awful.spawn("emacsclient -c -a \"\"") end,
			  {description = "Emacs Client", group = "launcher"}),
    awful.key({modkey, "Shift"}, "d", function () awful.spawn("emacsclient -c -a \"\" -e '(find-file \"~\")'") end,
			  {description = "Dired", group = "launcher"}),



    awful.key({}, "XF86AudioRaiseVolume", function () mypulse:raise() end,
			  {description = "Raise volume", group = "launcher"}),

    awful.key({}, "XF86AudioLowerVolume", function () mypulse:lower() end,
			  {description = "Lower volume", group = "launcher"}),

    awful.key({}, "XF86AudioMute", function () mypulse:mute() end,
			  {description = "Mute output", group = "launcher"}),


    awful.key({modkey}, "XF86AudioRaiseVolume", function () awful.spawn("mpc volume +2", false) end,
			  {description = "Raise mpd volume", group = "launcher"}),
    awful.key({modkey}, "XF86AudioLowerVolume", function () awful.spawn("mpc volume -2", false) end,
			  {description = "Lower mpd volume", group = "launcher"}),
    awful.key({modkey}, "XF86Launch1", function () awful.spawn("mpc toggle", false) end,
			  {description = "Toggle mpd", group = "launcher"}),


    awful.key({}, "XF86MonBrightnessUp", function () awful.spawn("xbacklight -inc 10") end,
			  {description = "Increase monitor brightness", group = "launcher"}),
    awful.key({}, "XF86MonBrightnessDown", function () awful.spawn("xbacklight -dec 10") end,
			  {description = "Increase monitor brightness", group = "launcher"}),
    awful.key({modkey, "Control"}, "l", function () awful.spawn.with_shell("sleep 0.08; xset dpms force off") end,
			  {description = "Turn off backlight", group = "launcher"}),


    awful.key({}, "XF86KbdBrightnessUp", function () awful.spawn.with_shell(
		"echo + > /run/kbdbacklight") end,
			  {description = "Increase keyboard brightness", group = "launcher"}),
    awful.key({}, "XF86KbdBrightnessDown", function () awful.spawn.with_shell(
		"echo - > /run/kbdbacklight") end,
			  {description = "Increase keyboard brightness", group = "launcher"}),


    awful.key({}, "XF86PowerOff", function () awful.spawn.with_shell("~/.config/rofi/powermenu/type-4/powermenu.sh") end,
			  {description = "Show powermenu", group = "launcher"}),
    awful.key({}, "XF86TouchpadToggle", function () awful.spawn("toggletpad") end,
			  {description = "Toggle touchpad", group = "launcher"}),
    awful.key({}, "XF86Launch1", function () awful.spawn("speedcrunch", true) end,
			  {description = "Toggle touchpad", group = "launcher"}),
    awful.key({}, "XF86Sleep", function () awful.spawn("lock") end,
			  {description = "Lock", group = "launcher"})

)
