local lain = require("lain")
local awful = require("awful")
local naughty = require("naughty")
local gears = require("gears")

local function get()
	local mic = lain.widget.pulse {
		settings = function()
			if volume_now.muted[2] == "yes" then
				widget:set_markup("")
			else
				widget:set_markup("")
			end
		end,
		timeout = 100000,
		devicetype = "source",
		cmd = "pactl list sources"
	}
	function mic:mute()
		awful.spawn(string.format("pactl set-source-mute 0 toggle"), false)
		gears.timer {
			timeout   = 0.05,
			call_now  = false,
			autostart = true,
			callback  = function()
				awful.spawn.easy_async("pactl get-source-mute 0", function(stdout, stderr, reason, exit_code)
										   naughty.notify { text = stdout, timeout = 1, height = 40, width = 130} end)
			end,
			single_shot = true,
		}
		self:update()
	end
	mic.widget:buttons(awful.util.table.join(
						   awful.button({}, 1, function() mic:mute() end)
	))
	return mic
end

return get()
