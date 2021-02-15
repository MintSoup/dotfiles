local awful = require("awful")
local gears = require("gears")

return {
	up = function()
		if b8 then
			mypulse:raise()
		end
	end,

	down = function()
		if b8 then
			mypulse:lower()
		end
	end
}
