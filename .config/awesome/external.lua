local awful = require("awful")
local gears = require("gears")

terminal = os.getenv("TERMINAL")

return gears.table.join(
    awful.key({modkey}, "Return", function () awful.spawn(terminal) end,
			  {description = "ST", group = "launcher"}),
    awful.key({modkey}, "space", function () awful.spawn("rofi -theme drun.rasi -show drun") end,
			  {description = "Rofi", group = "launcher"}),
    awful.key({modkey}, "z", function () awful.spawn("dmenuprojects") end,
			  {description = "Run project", group = "launcher"}),
    awful.key({modkey}, ";", function () awful.spawn("rofi -show emoji") end,
			  {description = "Insert emojis", group = "launcher"}),
	
    awful.key({modkey, "Shift"}, "e", function () awful.spawn("pcmanfm-qt") end,
			  {description = "PCManFM", group = "launcher"}),
    awful.key({modkey, "Shift"}, "s", function () awful.spawn("flameshot gui", false) end,
			  {description = "Flameshot", group = "launcher"}),
    awful.key({modkey, "Shift"}, "m", function () awful.spawn(terminal .. " -e ncmpcpp") end,
			  {description = "ncmpcpp", group = "launcher"}),
    awful.key({modkey, "Shift"}, "n", function () awful.spawn(terminal .. " -e sh -c \"nmtui\" ") end,
			  {description = "PCManFM", group = "launcher"}),
    awful.key({modkey, "Shift"}, "v", function () awful.spawn(terminal .. " -e vim") end,
			  {description = "Vim", group = "launcher"}),

	
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

	
    awful.key({}, "XF86KbdBrightnessUp", function () awful.spawn.with_shell(
		"echo + > /run/kbdbacklight") end,
			  {description = "Increase keyboard brightness", group = "launcher"}),
    awful.key({}, "XF86KbdBrightnessDown", function () awful.spawn.with_shell(
		"echo - > /run/kbdbacklight") end,
			  {description = "Increase keyboard brightness", group = "launcher"}),

	
    awful.key({}, "XF86PowerOff", function () awful.spawn("powermenu") end,
			  {description = "Show powermenu", group = "launcher"}),
    awful.key({}, "XF86TouchpadToggle", function () awful.spawn("toggletpad") end,
			  {description = "Toggle touchpad", group = "launcher"}),
    awful.key({}, "XF86Launch1", function () awful.spawn("speedcrunch", true) end,
			  {description = "Toggle touchpad", group = "launcher"}),
    awful.key({}, "XF86Sleep", function () awful.spawn("lock") end,
			  {description = "Lock", group = "launcher"})

)
