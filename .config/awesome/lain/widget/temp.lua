--[[

     Licensed under GNU General Public License v2
      * (c) 2013, Luca CPZ

--]]

local helpers  = require("lain.helpers")
local wibox    = require("wibox")
local tonumber = tonumber

-- {thermal,core} temperature info
-- lain.widget.temp

local function factory(args)
    args           = args or {}

    local temp     = { widget = args.widget or wibox.widget.textbox() }
    local timeout  = args.timeout or 30
    local tempfile = args.tempfile or "/sys/devices/virtual/thermal/thermal_zone0/temp"
    local settings = args.settings or function() end

    function temp.update()
		coretemp_now = helpers.first_line(tempfile)
		widget = temp.widget
		settings()
    end

    helpers.newtimer("thermal", timeout, temp.update)

    return temp
end

return factory
