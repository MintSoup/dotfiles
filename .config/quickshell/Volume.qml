import QtQuick
import Quickshell.Io
import Quickshell.Services.Pipewire
import Quickshell.Widgets

WrapperMouseArea {
    id: root

    readonly property PwNode sinkNode: Pipewire.defaultAudioSink
    readonly property bool muted: sinkNode?.audio?.muted ?? false
	readonly property real volume: sinkNode?.audio ? sinkNode.audio.volume * 100 : 0

    cursorShape: Qt.PointingHandCursor

    acceptedButtons: Qt.LeftButton | Qt.RightButton

    onClicked: mouse => {
        if (mouse.button === Qt.RightButton) {
            if (root.sinkNode?.audio)
                root.sinkNode.audio.muted = !root.sinkNode.audio.muted
        } else {
            pavucontrolProcess.running = true
        }
    }

    WheelHandler {
        acceptedDevices: PointerDevice.Mouse | PointerDevice.TouchPad
        onWheel: event => {
            if (!root.sinkNode?.audio) return
            const step = 0.01
            const newVol = Math.max(0, Math.min(1, root.sinkNode.audio.volume + (event.angleDelta.y > 0 ? step : -step)))
            root.sinkNode.audio.volume = newVol
        }
    }

    PwObjectTracker {
        objects: [root.sinkNode]
    }

    Process {
        id: pavucontrolProcess
        command: ["pavucontrol"]
    }

	Noto {
		text: {
			let icon
			if (root.muted) icon = " "
			else if (root.volume === 0) icon = ""
			else icon = ""
			return icon + " " + Math.round(root.volume)
		}
	}
}
