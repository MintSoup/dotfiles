pragma Singleton
import Quickshell
import QtQuick

Singleton {
	id: root
	readonly property string font: "Noto Sans Nerd Font"
	readonly property real fontSize: 16
	readonly property color bg: "#1e222a"
	readonly property color fg: "#e8e8e8"
	readonly property color active_ws: "#51afef"
}
