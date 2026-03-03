import QtQuick
import Quickshell.Io
import Quickshell.Widgets

WrapperItem {
    id: root

    property real usedGb: 0

    FileView {
        id: memFile
        path: "/proc/meminfo"
        blockLoading: true
    }

    Timer {
        interval: 1000
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: {
            memFile.reload()
            const txt = memFile.text()
            const total = parseInt(txt.match(/MemTotal:\s+(\d+)/)[1])
            const avail = parseInt(txt.match(/MemAvailable:\s+(\d+)/)[1])
            root.usedGb = ((total - avail) / 1048576)
        }
    }

    Noto {
        text: " " + root.usedGb.toFixed(1) + "G"
    }
}
