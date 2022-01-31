local lain = require("lain")
local awful = require("awful")
local watch = awful.spawn and awful.spawn.with_line_callback
local gears = require("gears")

local function lookup_by_limits(limits, value)
    if type(limits) == "table" then
        local last = nil
        if value then
            for k, v in ipairs(limits) do
                if (value <= v[1]) then
                    return v[2]
                end
                last = v[2]
            end
        end
        return last
    else
        return limits
    end
end

local function geticon(status, capacity)
	if status == "Not charging" then
		return " "
	elseif status == "Charging" then
		return " "
	else
	return lookup_by_limits({
		{10,    " "},
		{20,    " "},
		{30,    " "},
		{40,    " "},
		{50,    " "},
		{60,    " "},
		{70,    " "},
		{80,    " "},
		{90,    " "},
		{999,   " "}},
		capacity)
	end
end

local function get()
	local tt = awful.tooltip{}


	local widget = lain.widget.bat {
		settings = function()
			widget:set_markup(geticon(bat_now.status, bat_now.perc) .. bat_now.perc)
			tt.text = bat_now.time .. "m remaining"
		end,
		battery = "BAT0",
		timeout = 30,
		notify = "off",
	}


	tt.add_to_object(widget.widget)

	widget.widget:connect_signal('mouse::enter', function()
		tt.visible = true
	end)
	widget.widget:connect_signal('mouse::leave', function()
		tt.visible = false
	end)

	widget.listener = watch("acpi_listen", {
		stdout = function(line)
			gears.timer {
				timeout   = 2.5,
				call_now  = true,
				autostart = true,
				callback  = widget.update,
				single_shot = true,
			}
		end
	})

	awesome.connect_signal("exit", function()
		awesome.kill(widget.listener, awesome.unix_signal.SIGTERM)
	end)

	return widget
end


return get()
