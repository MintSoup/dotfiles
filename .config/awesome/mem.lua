local awful = require("awful")
local lain = require("lain")
local fs = require("gears.filesystem")
local table = require("gears.table")

local function get()
	local mem = lain.widget.mem {
		settings = function()
			local ram = tonumber(string.format("%.1f", mem_now.used / 1024))
			widget:set_markup(" " .. ram .. "G")
		end,
		timeout = 5,
	}


	mem.widget:buttons(
		awful.button({}, 1, function() mem:update() end)	
	)

	return mem
end

return get()
