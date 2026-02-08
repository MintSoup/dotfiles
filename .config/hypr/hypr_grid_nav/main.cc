#include <hyprland/src/plugins/PluginAPI.hpp>
#include <hyprland/src/desktop/state/FocusState.hpp>
#include <hyprland/src/helpers/Monitor.hpp>
#include <hyprland/src/devices/IPointer.hpp>
#include <hyprland/src/managers/KeybindManager.hpp>

#include <string>

static HANDLE PHANDLE = nullptr;

static void dbg(const std::string &msg) {
	HyprlandAPI::addNotification(PHANDLE, msg, CHyprColor{1.0, 0.2, 0.2, 1.0},
								 1000);
}

static bool button8_held = false;

static inline WORKSPACEID ws_from_grid(WORKSPACEID row, WORKSPACEID col) {
	return row * 3 + col + 1;
}

static inline WORKSPACEID ws_row(WORKSPACEID ws) {
	return (ws - 1) / 3;
}

static inline WORKSPACEID ws_col(WORKSPACEID ws) {
	return (ws - 1) % 3;
}

static inline void switch_workspace(WORKSPACEID ws) {
	g_pKeybindManager->m_dispatchers["workspace"](std::to_string(ws));
}

static void on_mouse_button(void *, SCallbackInfo &, std::any data) {
	auto ev = std::any_cast<IPointer::SButtonEvent>(data);

	const unsigned int button = ev.button;
	const bool pressed = (ev.state == WL_POINTER_BUTTON_STATE_PRESSED);

	if (button == 276) {
		button8_held = pressed;
		return;
	}

	if (!button8_held)
		return;

	if (!pressed)
		return;

	int ws = Desktop::focusState()->monitor()->activeWorkspaceID();
	int row = ws_row(ws);
	int col = ws_col(ws);

	if (button == 272 && col > 0) {
		switch_workspace(ws_from_grid(row, col - 1));
	} else if (button == 273 && col < 2) {
		switch_workspace(ws_from_grid(row, col + 1));
	}
}

static void on_mouse_axis(void *, SCallbackInfo &, std::any data) {
	auto emap = std::any_cast<std::unordered_map<std::string, std::any>>(data);
	auto ev = std::any_cast<IPointer::SAxisEvent>(emap.at("event"));

	if (!button8_held)
		return;

	if (ev.axis != WL_POINTER_AXIS_VERTICAL_SCROLL)
		return;
	if (ev.delta == 0)
		return;

	int ws = Desktop::focusState()->monitor()->activeWorkspaceID();
	int row = ws_row(ws);
	int col = ws_col(ws);

	if (ev.delta < 0 && row < 2) {
		switch_workspace(ws_from_grid(row + 1, col));
	} else if (ev.delta > 0 && row > 0) {
		switch_workspace(ws_from_grid(row - 1, col));
	}
}

APICALL EXPORT std::string PLUGIN_API_VERSION() {
	return HYPRLAND_API_VERSION;
}

APICALL EXPORT PLUGIN_DESCRIPTION_INFO PLUGIN_INIT(HANDLE handle) {
	PHANDLE = handle;

	const std::string COMPOSITOR_HASH = __hyprland_api_get_hash();
	const std::string CLIENT_HASH = __hyprland_api_get_client_hash();
	if (COMPOSITOR_HASH != CLIENT_HASH) {
		HyprlandAPI::addNotification(
			PHANDLE, "[hypr-grid-nav] Mismatched headers. Cannot load.",
			CHyprColor{1.0, 0.2, 0.2, 1.0}, 5000);
		throw std::runtime_error("[hypr-grid-nav] Version mismatch");
	}

	static auto btn = HyprlandAPI::registerCallbackDynamic(
		PHANDLE, "mouseButton", on_mouse_button);
	static auto axis = HyprlandAPI::registerCallbackDynamic(
		PHANDLE, "mouseAxis", on_mouse_axis);

	return {.name = "hypr-grid-nav",
			.description = "3x3 grid workspaces",
			.author = "MintSoup",
			.version = "0.1.0"};
}

APICALL EXPORT void PLUGIN_EXIT() {
}
