import QtQuick
import QtQuick.Controls
import Quickshell.Services.Pipewire
import Quickshell.Widgets

WrapperMouseArea {
    id: root

    readonly property PwNode micNode: Pipewire.defaultAudioSource
    readonly property bool muted: micNode?.audio?.muted ?? true

    cursorShape: Qt.PointingHandCursor

    onClicked: {
		console.log(micNode)
        if (root.micNode?.audio)
            root.micNode.audio.muted = !root.micNode.audio.muted
    }

	PwObjectTracker {
        objects: [root.micNode]
    }

    Noto {
        text: root.muted ? "" : ""
    }
}
