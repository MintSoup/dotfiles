import QtQuick
import Quickshell.Io
import Quickshell.Hyprland
import Quickshell.Widgets

WrapperMouseArea {
	id: root

	property string layout: ""

	cursorShape: Qt.PointingHandCursor
	onClicked: Hyprland.dispatch("switchxkblayout all next")

	function refresh() { devicesProcess.running = true }

	Process {
		id: devicesProcess
		command: ["hyprctl", "devices", "-j"]
		running: true
		stdout: StdioCollector {
			onStreamFinished: {
				const data = JSON.parse(text)
				const kb = data.keyboards.find(k => k.main) ?? data.keyboards[0]
				const layouts = kb.layout.split(",")
				root.layout = layouts[kb.active_layout_index] ?? layouts[0]
			}
		}
	}

	Connections {
		target: Hyprland
		function onRawEvent(event) {
			if (event.name !== "activelayout") return
			root.refresh()
		}
	}

	Noto {
		text: root.layout
	}
}
