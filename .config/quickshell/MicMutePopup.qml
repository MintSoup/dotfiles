import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Services.Pipewire
import Quickshell.Hyprland

PanelWindow {
    id: root

    readonly property bool muted: Pipewire.defaultAudioSource?.audio?.muted ?? true
    property real popScale: 0.5

    exclusionMode: ExclusionMode.Ignore
    width: 192
    height: 192
    anchors.bottom: true
    margins.bottom: 128
    color: "transparent"
    visible: HyprlandWindow.opacity > 0
    HyprlandWindow.opacity: 0

    Behavior on HyprlandWindow.opacity {
        NumberAnimation { duration: 125; easing.type: Easing.OutCubic }
    }

    Behavior on popScale {
        NumberAnimation { duration: 125; easing.type: Easing.OutCubic }
    }

    Timer {
        id: fadeTimer
        interval: 400
        onTriggered: {
            root.HyprlandWindow.opacity = 0
            root.popScale = 0.5
        }
    }

    Connections {
        target: Pipewire.defaultAudioSource?.audio
        function onMutedChanged() {
            root.popScale = 1
            root.HyprlandWindow.opacity = 0.9
            fadeTimer.restart()
        }
    }

    Rectangle {
        anchors.fill: parent
        color: Theme.bg
        radius: 1000

        transform: Scale {
            origin.x: root.width / 2
            origin.y: root.height / 2
            xScale: root.popScale
            yScale: root.popScale
        }

        Noto {
            anchors.centerIn: parent
			text: root.muted ? "" : ""
            font.pixelSize: 128
            color: root.muted ? Theme.accent2 : Theme.accent1
        }
    }
}
