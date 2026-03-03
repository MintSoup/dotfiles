import QtQuick
import Quickshell.Io
import Quickshell.Widgets
WrapperItem {
	id: root

	visible: false
	property int capacity: 0
	property string status: ""

	readonly property string icon: {
		if (status === "Charging") return "󰂄"
		if (status === "Not charging") return "󱟢"
		if (capacity < 10) return ""
		if (capacity < 20) return ""
		if (capacity < 30) return ""
		if (capacity < 40) return ""
		if (capacity < 50) return ""
		if (capacity < 60) return ""
		if (capacity < 70) return ""
		if (capacity < 80) return ""
		if (capacity < 90) return ""
		return ""
	}

	FileView {
		id: capacityFile
		path: "/sys/class/power_supply/BAT0/capacity"
		blockLoading: true
		onLoadFailed: root.visible = false
		onLoaded: root.visible = true
	}

	FileView {
		id: statusFile
		path: "/sys/class/power_supply/BAT0/status"
		blockLoading: true
	}

	Timer {
		interval: 1000
		running: root.visible
		repeat: true
		triggeredOnStart: true
		onTriggered: {
			capacityFile.reload()
			statusFile.reload()
			root.capacity = parseInt(capacityFile.text())
			root.status = statusFile.text().trim()
		}
	}

	Noto {
		text: root.icon + " " + root.capacity
	}
}
