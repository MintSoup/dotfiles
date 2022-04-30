-- Standard awesome library

local gears = require("gears")
local shape = gears.shape
local awful = require("awful")
require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
-- Notification library
local naughty = require("naughty")
local hotkeys_popup = require("awful.hotkeys_popup")
local separators = require("lain").util.separators
local scroll = require("scroll")

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
	naughty.notify({ preset = naughty.config.presets.critical,
					 title = "Oops, there were errors during startup!",
					 text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
	local in_error = false
	awesome.connect_signal("debug::error", function (err)
							   -- Make sure we don't go into an endless error loop
							   if in_error then return end
							   in_error = true

							   naughty.notify({ preset = naughty.config.presets.critical,
												title = "Oops, an error happened!",
												text = tostring(err) })
							   in_error = false
	end)
end
-- }}}


-- {{{ Variable definitions
-- Themes define colours, icons, font and wallpapers.

awesome_config_folder = gears.filesystem.get_xdg_config_home() .. "awesome/"

beautiful.init(awesome_config_folder .. "themes/mint/theme.lua")

modkey = "Mod4"




-- Autostart
local function run_once(cmd)
	if type(cmd) == "string" then
		awful.spawn.with_shell(string.format("pgrep -u $USER -x %s > /dev/null || %s", cmd, cmd))
	else
		awful.spawn.with_shell(string.format("pgrep -u $USER -x %s > /dev/null || %s", cmd[1], cmd[2]))
	end
end


run_once({"emacs", "emacs --daemon"})
run_once("brave")
run_once({"Discord", "discord"})
run_once({"electron", "element-desktop"})
-- run_once({"pia-client","/opt/piavpn/bin/pia-client"})
run_once("flameshot")
run_once("unclutter", "unclutter --timeout 30")
run_once("xbindkeys")
run_once("copyq")
run_once({"picom", "picom --experimental-backends --backend glx -b "})
run_once("numlockx on")
run_once("xsettingsd")

awful.spawn("xset r rate 200 40", false)


-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.layouts = {
	awful.layout.suit.tile,
	awful.layout.suit.max,
}
-- }}}

-- Keyboard map indicator and switcher
mykeyboardlayout = awful.widget.keyboardlayout()

wallpapers = {
	"/home/areg/Wallpapers/castle.jpg",
}

math.randomseed(os.time() * 4214053)

local function set_wallpaper(s)
	local wallpaper = wallpapers[math.random(1, #wallpapers)]
	gears.wallpaper.maximized(wallpaper, s, true)
end


-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", set_wallpaper)

-- Global widgets
local mycalendar = wibox.container.background(wibox.widget.textclock(" %a, %d %b %Y"))
local myclock = wibox.widget.textclock(" %R:%S", 1)
local mybattery = require("battery")
local mytemp = require("temp")
local mymem = require("mem")
mypulse = require("pulse") -- not local so external.lua can access
mymic = require("mic")



awful.screen.connect_for_each_screen(function(s)
		-- Wallpaper
		set_wallpaper(s)

		-- Each screen has its own tag table.
		awful.tag({" ", " ", " ", " ", " "}, s, awful.layout.layouts[1])

		awful.tag({" ", " ", " "}, s, awful.layout.suit.max)

		awful.tag({"﫸 " }, s, awful.layout.layouts[1])


		-- Create a taglist widget
		s.mytaglist = awful.widget.taglist {
			screen  = s,
			filter = function (t) return t.selected or #t:clients() > 0 end,
			-- filter  = awful.widget.taglist.filter.all,
			buttons = {
				awful.button({ }, 1, function(t) t:view_only() end),
				awful.button({ modkey }, 1, function(t)
						if client.focus then
							client.focus:move_to_tag(t)
						end
				end),
				awful.button({ }, 3, awful.tag.viewtoggle),
				awful.button({ modkey }, 3, function(t)
						if client.focus then
							client.focus:toggle_tag(t)
						end
				end),
			}
		}


		-- Create a tasklist widget
		s.mytasklist = awful.widget.tasklist {
			screen  = s,
			filter  = awful.widget.tasklist.filter.focused,
			buttons = tasklist_buttons
		}

		-- Create the wibox
		s.mywibox = awful.wibar{ position = "top", screen = s }

		local count = 2

		local function wwrapper(w)
			local tmp = wibox.container.background(wibox.container.margin(w, 9, 9))
			tmp.fg = beautiful.colors[count % #beautiful.colors + 1]
			count = count + 1
			return tmp
		end
		local function wwrapper_tight(w)
			local tmp = wibox.container.background(w)
			tmp.fg = beautiful.colors[count % #beautiful.colors + 1]
			count = count + 1
			return tmp
		end

		separators.width = math.floor(s.mywibox.height * 0.5)

		-- Add widgets to the wibox
		s.mywibox:setup {
			layout = wibox.layout.align.horizontal,
			{ -- Left widgets
				layout = wibox.layout.fixed.horizontal,
				--wibox.container.background(s.mytaglist, beautiful.color1),
				s.mytaglist,
			},
			(s.mytasklist),

			{ -- Right widgets
				layout = wibox.layout.fixed.horizontal,
				-- wwrapper(wibox.widget.systray()),
				wwrapper_tight(mykeyboardlayout),
				wwrapper(mybattery.widget),
				wwrapper(mytemp.widget),
				wwrapper(mymem.widget),
				wwrapper(mypulse.widget),
				wwrapper(mymic.widget),
				wwrapper(mycalendar),
				wwrapper(myclock),
			},
		}

end)

-- Select middle tag
for s in screen do
	s.tags[5]:view_only()
	-- bar
	for t = 1, #s.tags do
		s.tags[t].bar = true
	end
end

-- {{{ Mouse bindings

-- }}}

-- {{{ Key bindings



globalkeys = gears.table.join(

	-- Layout
	awful.key({ modkey,           }, "j", function () awful.client.focus.byidx( 1) end,
		{description = "focus next by index", group = "client"}),
	awful.key({ modkey,           }, "k", function () awful.client.focus.byidx(-1) end,
		{description = "focus previous by index", group = "client"}),

	awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1) end,
		{description = "swap with next client by index", group = "client"}),
	awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1) end,
		{description = "swap with previous client by index", group = "client"}),

	awful.key({ modkey, "Mod1"   }, "h", function () awful.client.incwfact(	0.05) 	end,
		{description = "Increase window factor", group = "client"}),
	awful.key({ modkey, "Mod1"   }, "l", function () awful.client.incwfact(-0.05) 	end,
		{description = "Decrease window factor", group = "client"}),
	awful.key({ modkey, "Mod1"   }, "o", function () awful.client.setwfact( 0.5)	end,
		{description = "Reset window factor", group = "client"}),



	awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05) end,
		{description = "increase master width factor", group = "layout"}),
	awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05) end,
		{description = "decrease master width factor", group = "layout"}),

	awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1, nil, true) end,
		{description = "increase the number of master clients", group = "layout"}),
	awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1, nil, true) end,
		{description = "decrease the number of master clients", group = "layout"}),

	awful.key({ modkey,           }, "m", function () awful.layout.inc( 1) end,
		{description = "select next", group = "layout"}),
	awful.key({ modkey,           }, "u", awful.client.urgent.jumpto,
		{description = "jump to urgent client", group = "client"}),
	-- awesome
	awful.key({ modkey, "Control" }, "r", awesome.restart,
		{description = "reload awesome", group = "awesome"}),
	awful.key({ modkey,           }, "s",      hotkeys_popup.show_help,
		{description="show help", group="awesome"}),
	awful.key({modkey,			},	 "b", function()
			-- local wibox = awful.client.focus.object.screen.mywibox
			local wibox = mouse.screen.mywibox
			wibox.visible = not wibox.visible
			awful.tag.selected().bar = wibox.visible
	end,
		{description="toggle bar", group="awesome"})
)

clientkeys = gears.table.join(
	awful.key({ modkey			}, "x", function (c) c:kill() end,
		{description = "close", group = "client"}),
	awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle,
		{description = "toggle floating", group = "client"}),
	awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end,
		{description = "move to master", group = "client"}),
	awful.key({ modkey, "Control"   }, "m",
		function (c)
			c.maximized = not c.maximized
		end,
		{description="Unmaximize client", group="client"}),
    awful.key({ modkey, }, "o", function (c) c:move_to_screen() end,
		{description = "move to screen", group = "client"})
)

local np_map = { 87, 88, 89, 83, 84, 85, 79, 80, 81 }

for i = 1, 9 do
	globalkeys = gears.table.join(globalkeys,
								  -- View tag only.
								  awful.key({ modkey }, "#" .. np_map[i],
									  function ()
										  local screen = awful.screen.focused()
										  local tag = screen.tags[i]
										  if tag then
											  tag:view_only()
										  end
									  end,
									  {description = "view tag #"..i, group = "tag"}),
								  -- Toggle tag display.
								  awful.key({ modkey, "Control" }, "#" .. np_map[i],
									  function ()
										  local screen = awful.screen.focused()
										  local tag = screen.tags[i]
										  if tag then
											  awful.tag.viewtoggle(tag)
										  end
									  end,
									  {description = "toggle tag #" .. i, group = "tag"}),
								  -- Move client to tag.
								  awful.key({ modkey, "Shift" }, "#" .. np_map[i],
									  function ()
										  if client.focus then
											  local tag = client.focus.screen.tags[i]
											  if tag then
												  client.focus:move_to_tag(tag)
											  end
										  end
									  end,
									  {description = "move focused client to tag #"..i, group = "tag"}),
								  -- Toggle tag on focused client.
								  awful.key({ modkey, "Control", "Shift" }, "#" .. np_map[i],
									  function ()
										  if client.focus then
											  local tag = client.focus.screen.tags[i]
											  if tag then
												  client.focus:toggle_tag(tag)
											  end
										  end
									  end,
									  {description = "toggle focused client on tag #" .. i, group = "tag"})
	)
end

gears.table.merge(globalkeys, require("external"))


clientbuttons = gears.table.join(
	awful.button({ }, 1, function (c)
			c:emit_signal("request::activate", "mouse_click", {raise = true})
	end),
	awful.button({ modkey }, 1, function (c)
			c:emit_signal("request::activate", "mouse_click", {raise = true})
			awful.mouse.client.move(c)
	end),
	awful.button({ modkey }, 3, function (c)
			c:emit_signal("request::activate", "mouse_click", {raise = true})
			awful.mouse.client.resize(c)
	end),
	awful.button({ modkey }, 2, function (c)
			c:emit_signal("request::activate", "mouse_click", {raise = true})
			awful.client.floating.toggle()
	end)
)

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
	-- All clients will match this rule.
	{ rule = { },
	  properties = { border_width = beautiful.border_width,
					 border_color = beautiful.border_normal,
					 focus = awful.client.focus.filter,
					 raise = true,
					 keys = clientkeys,
					 buttons = clientbuttons,
					 screen = awful.screen.preferred,
					 placement = awful.placement.no_overlap+awful.placement.no_offscreen
	  }
	},

	-- Floating clients.
	{ rule_any = {
		  instance = {
		  },
		  class = {
			  "Pavucontrol",
			  "SpeedCrunch",
			  "copyq",
		  },
		  name = {

		  },
		  role = {
			  "pop-up",       -- e.g. Google Chrome's (detached) Developer Tools.
		  }
	}, properties = { floating = true }},

	{ rule = { class = "Brave-browser" }, properties = { screen = 1, tag = " " } },
	{ rule = { class = "Element" }, properties = { screen = 1, tag = " " } },
	{ rule = { class = "discord" }, properties = { screen = 1, tag = " " } },
	{ rule = { class = "st-256color" }, properties = { size_hints_honor = false } },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.

function is_terminal(c)
	return c.class == "st-256color"
end

function copy_size(c, parent_client, idx)
	if not c or not parent_client then
		return
	end
	if not c.valid or not parent_client.valid then
		return
	end

	c.floating = parent_client.floating
	if c.floating then
		c.x = parent_client.x;
		c.y = parent_client.y;
		c.width = parent_client.width;
		c.height = parent_client.height;
	else
		local d = idx - awful.client.idx(c).idx
		awful.client.swap.byidx(d, c)
	end
end




local gppid = 'sh '..awesome_config_folder..'helper.sh gppid '
local ppid = 'sh '..awesome_config_folder..'helper.sh ppid '
client.connect_signal("manage", function (c)

						  if awesome.startup
							  and not c.size_hints.user_position
							  and not c.size_hints.program_position then
							  -- Prevent clients from being unreachable after screen count changes.
							  awful.placement.no_offscreen(c)
						  end


						  if not is_terminal(c) then
							  local parent_client=awful.client.focus.history.get(c.screen, 1)
							  if not c.pid then return end
							  awful.spawn.easy_async(gppid .. c.pid, function(gppid)
														 awful.spawn.easy_async(ppid .. c.pid, function(ppid)
																					if parent_client and parent_client.pid and (gppid:find('^' .. parent_client.pid) or ppid:find('^' .. parent_client.pid)) and
																						is_terminal(parent_client) then

																						local idx
																						if not parent_client.floating then
																							idx = awful.client.idx(parent_client).idx
																						end

																						c:connect_signal("unmanage", function()
																											 parent_client.hidden = false

																											 if parent_client.floating then
																												 parent_client.x = c.x
																												 parent_client.y = c.y
																												 parent_client.width = c.width
																												 parent_client.height = c.height
																											 end
																						end)

																						parent_client.hidden = true
																						copy_size(c, parent_client, idx)
																						return
																					end
														 end)
							  end)
						  end
end)

fastscroll = {
	"Brave-browser",
	"discord",
	"Element",
	"Zathura",
}


slowonly = false

function fast()
	awful.spawn.with_shell("echo 3 > /tmp/libinput_discrete_deltay_multiplier")
end
function slow()
	awful.spawn.with_shell("echo 1 > /tmp/libinput_discrete_deltay_multiplier")
end

function syncon(c)
	if not c or slowonly then
		slow()
		return
	end

	for i, n in ipairs(fastscroll) do
		if c.class == n then
			fast()
			return
		end
	end
	slow()
end

function resync()
	syncon(awful.mouse.object.get_current_client())
end


-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal("mouse::enter", function(c)
						  -- c:emit_signal("request::activate", "mouse_enter", {raise = false})
						  c:activate({
								  context = "mouse_enter",
								  raise = false
						  })
						  syncon(c)
end)

client.connect_signal("mouse::leave", resync)
client.connect_signal("focus", function(c)
						  c.border_color = beautiful.border_focus
						  local x = mouse.coords().x
						  local y = mouse.coords().y
						  gears.timer {
							  timeout   = 0.001,
							  single_shot = true,
							  autostart = true,
							  callback  = function()
								  if c.valid then
									  if x >= c.x and x < c.x + c.width and y >= c.y and y < c.y + c.height then
										  c:emit_signal("mouse::enter")
									  end
								  end
							  end
						  }
end)

client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)

client.connect_signal("property::floating", function(c)
						  if c.maximized or c.fullscreen then return end

						  c.size_hints_honor = c.floating
						  c.above = c.floating

end)

client.connect_signal("property::minimized", function(c) c.minimized = false end)

function left()
	local tags = awful.screen.focused().selected_tags
	if #tags > 1 then return end
	local tag = tags[1]
	if (tag.index - 1) % 3 > 0 then
		awful.tag.viewidx(-1)
	end
end

function right()
	local tags = awful.screen.focused().selected_tags
	if #tags > 1 then return end
	local tag = tags[1]

	if (tag.index - 1) % 3 < 2 then
		awful.tag.viewidx(1)
	end
end


function up()
	local tags = awful.screen.focused().selected_tags
	if #tags > 1 then return end
	local tag = tags[1]
	if tag.index < 7 then
		awful.tag.viewidx(3)
	end
end

function down()
	local tags = awful.screen.focused().selected_tags
	if #tags > 1 then return end
	local tag = tags[1]
	if tag.index > 3 then
		awful.tag.viewidx(-3)
	end
end






awful.tag.attached_connect_signal(nil, "property::selected", function(t)
									  if not t.selected then return end
									  local wibox = mouse.screen.mywibox
									  wibox.visible = t.bar
end)

screen.connect_signal("arrange", function (s)
						  if not s.selected_tag then
							  return
						  end
						  local max = s.selected_tag.layout.name == "max"
						  local only_one = #s.tiled_clients == 1 -- use tiled_clients so that other floating windows don't affect the count
						  -- but iterate over clients instead of tiled_clients as tiled_clients doesn't include maximized windows
						  for _, c in pairs(s.clients) do
							  if (max or only_one) and not c.floating or c.maximized then
								  c.border_width = 0
							  else
								  c.border_width = beautiful.border_width
							  end
						  end
end)

naughty.config.defaults.border_width = beautiful.notification_border_width

-- }}}
