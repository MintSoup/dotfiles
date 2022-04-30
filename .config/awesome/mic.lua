local lain = require("lain")
local awful = require("awful")

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

		self:update()
	end
	mic.widget:buttons(awful.util.table.join(
						   awful.button({}, 1, function() mic:mute() end)
	))
	return mic
end

return get()
