import QtQuick
import Quickshell.Io
import Quickshell.Widgets

WrapperItem {
    id: root

    property int temp: 0
    readonly property string icon: {
        if (temp < 50) return ""
        if (temp < 70) return ""
        if (temp < 90) return ""
        return ""
    }

    FileView {
        id: tempFile
        path: "/sys/class/hwmon/hwmon7/temp7_input"
        blockLoading: true
    }

    Timer {
        interval: 1000
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: {
            tempFile.reload()
            root.temp = parseInt(tempFile.text()) / 1000
        }
    }

    Noto {
        text: root.icon + " " + root.temp + "°C"
    }
}
