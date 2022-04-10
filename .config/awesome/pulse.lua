local lain = require("lain")
local awful = require("awful")

local function get()
	local volume = lain.widget.pulse {
		settings = function()
			if volume_now.muted == "yes" then
				widget:set_markup("婢 " .. volume_now.left)
			elseif volume_now.left == "0" then
				widget:set_markup(" " .. volume_now.left)
			else
				widget:set_markup(" " .. volume_now.left)
			end
		end,
		timeout = 100000,
		cmd = string.format("pactl get-sink-volume %s", 0)
	}

	volume.delta = 2

	function volume:raise()
		local volume = tonumber(volume_now.left)

		if volume <= 100 - self.delta then
			awful.spawn(string.format("pactl set-sink-volume %s +%d%%", 0,
																		self.delta))
		else
			awful.spawn(string.format("pactl set-sink-volume %s 100%%", 0))
		end
		self:update()
	end
	function volume:lower()
		awful.spawn(string.format("pactl set-sink-volume %s -%d%%", 0,
																	self.delta), false)
		self:update()
	end
	function volume:mute()
		awful.spawn(string.format("pactl set-sink-mute %s toggle", 0), false)
		self:update()
	end

	volume.widget:buttons(awful.util.table.join(
		awful.button({}, 1, function() -- left click
			awful.spawn("pavucontrol")
		end),
		awful.button({}, 2, function() -- middle click
			awful.spawn(string.format("pactl set-sink-volume %s 100%%", volume.device))
			volume.update()
		end),
		awful.button({}, 3, function() -- right click
			volume:mute()
		end),
		awful.button({}, 4, function() -- scroll up
			volume:raise()
		end),
		awful.button({}, 5, function() -- scroll down
			volume:lower()
		end)
	))
	return volume
end

return get()
