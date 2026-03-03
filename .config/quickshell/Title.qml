import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Hyprland
import Quickshell.Widgets

WrapperItem {
    id: root

	readonly property HyprlandToplevel activeToplevel: {
		const ws = Hyprland.focusedWorkspace
		if (!ws || ws.toplevels.values.length === 0) return null
		const t = Hyprland.toplevels.values.find(t => t.activated)
		return t !== undefined ? t : null
	}

    readonly property string appId: activeToplevel?.lastIpcObject?.class ?? activeToplevel?.wayland?.appId ?? ""
    readonly property string title: activeToplevel?.title ?? ""
	readonly property var entry: appId !== "" ? DesktopEntries.heuristicLookup(appId) : null

    RowLayout {
        spacing: 4
        IconImage {
            source: Quickshell.iconPath(root.entry?.icon ?? "", true)
            implicitSize: 20
            mipmap: true
            visible: source !== ""
            Layout.alignment: Qt.AlignVCenter
        }
        Noto {
            text: root.title
            Layout.alignment: Qt.AlignVCenter
        }
    }
}
