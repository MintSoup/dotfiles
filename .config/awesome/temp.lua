local awful = require("awful")
local lain = require("lain")
local fs = require("gears.filesystem")
local table = require("gears.table")

local function findfile()
	return table.iterate({
 		"/sys/bus/platform/devices/coretemp.0/hwmon/hwmon1/temp7_input",
 		"/sys/bus/platform/devices/coretemp.0/hwmon/hwmon2/temp7_input",
 		"/sys/bus/platform/devices/coretemp.0/hwmon/hwmon3/temp7_input",
 		"/sys/bus/platform/devices/coretemp.0/hwmon/hwmon4/temp7_input",
 		"/sys/bus/platform/devices/coretemp.0/hwmon/hwmon5/temp7_input",
 		"/sys/bus/platform/devices/coretemp.0/hwmon/hwmon6/temp7_input",
 		"/sys/bus/platform/devices/coretemp.0/hwmon/hwmon7/temp7_input",
	}, fs.file_readable)():gsub("temp7","temp1")
end

local function get()
	local temp = lain.widget.temp {
		settings = function()
			widget:set_markup(" " .. string.sub(coretemp_now, 1, 2) .. "°C")
		end,
		tempfile = findfile(),
		timeout = 1,
	}
	temp.widget:buttons(
		awful.button({}, 1, function() awful.spawn(terminal .. " -e htop") end)	
	)

	return temp
end

return get()
