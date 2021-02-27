local awful = require("awful")
local lain = require("lain")
local fs = require("gears.filesystem")
local table = require("gears.table")

local function get()
	local mem = lain.widget.mem {
		settings = function()
			local ram = tonumber(string.format("%.2f", mem_now.used / 1024))
			widget:set_markup(" " .. ram .. "GiB")
		end,
		timeout = 5,
	}
	return mem
end

return get()
