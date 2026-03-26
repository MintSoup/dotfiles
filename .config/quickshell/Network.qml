import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Io
import Quickshell.Widgets

WrapperMouseArea {
    id: root

    property var interfaces: []
    property string selectedIface: ""
    property real currentRx: 0
    property real currentTx: 0
    readonly property int maxSamples: 300
    readonly property int pollInterval: 200
    property bool popupOpen: false
    property var _data: ({})

    cursorShape: Qt.PointingHandCursor
    onClicked: {
        root.popupOpen = !root.popupOpen
        if (root.popupOpen) graph.requestPaint()
    }

    function formatSpeed(bps) {
        if (bps >= 1073741824) return (bps / 1073741824).toFixed(1) + " GB/s"
        if (bps >= 1048576) return (bps / 1048576).toFixed(1) + " MB/s"
        if (bps >= 1024) return (bps / 1024).toFixed(0) + " KB/s"
        return Math.round(bps) + " B/s"
    }

    function selectIface(name) {
        root.selectedIface = name
        var d = root._data[name]
        if (d) {
            root.currentRx = d.currentRx
            root.currentTx = d.currentTx
        } else {
            root.currentRx = 0
            root.currentTx = 0
        }
        graph.requestPaint()
    }

    FileView {
        id: netFile
        path: "/proc/net/dev"
        blockLoading: true
    }

    Timer {
        interval: root.pollInterval
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: {
            netFile.reload()
            var txt = netFile.text()
            var lines = txt.split('\n').slice(2)
            var data = root._data
            var ifaces = []

            for (var li = 0; li < lines.length; li++) {
                var m = lines[li].match(/^\s*(\S+):\s+(\d+)(?:\s+\d+){7}\s+(\d+)/)
                if (!m || m[1] === 'lo') continue

                var name = m[1]
                var rx = parseInt(m[2])
                var tx = parseInt(m[3])
                ifaces.push(name)

                if (!data[name]) {
                    data[name] = {
                        rxHistory: [],
                        txHistory: [],
                        lastRx: -1,
                        lastTx: -1,
                        currentRx: 0,
                        currentTx: 0
                    }
                }

                var d = data[name]
                if (d.lastRx >= 0) {
                    var rxRate = Math.max(0, rx - d.lastRx) * (1000 / root.pollInterval)
                    var txRate = Math.max(0, tx - d.lastTx) * (1000 / root.pollInterval)
                    d.currentRx = rxRate
                    d.currentTx = txRate
                    d.rxHistory.push(rxRate)
                    d.txHistory.push(txRate)
                    if (d.rxHistory.length > root.maxSamples) d.rxHistory.shift()
                    if (d.txHistory.length > root.maxSamples) d.txHistory.shift()
                }
                d.lastRx = rx
                d.lastTx = tx
            }

            var sel = data[root.selectedIface]
            if (sel) {
                root.currentRx = sel.currentRx
                root.currentTx = sel.currentTx
            }

            if (JSON.stringify(ifaces) !== JSON.stringify(root.interfaces)) {
                root.interfaces = ifaces
                if (!root.selectedIface || ifaces.indexOf(root.selectedIface) < 0) {
                    root.selectedIface = ifaces.length > 0 ? ifaces[0] : ""
                }
            }

            if (root.popupOpen) graph.requestPaint()
        }
    }

    Noto {
        text: "󰈀"
    }

    PanelWindow {
        id: popup
        visible: popupProgress > 0
        color: "transparent"
        exclusionMode: ExclusionMode.Ignore
        width: 560
        height: 320
        anchors {
            top: true
            right: true
        }
        margins {
            top: 36
            right: 8
        }

        property real popupProgress: root.popupOpen ? 1 : 0
        Behavior on popupProgress {
            NumberAnimation { duration: 200; easing.type: Easing.OutCubic }
        }

        Item {
            anchors.fill: parent
            clip: true

            Rectangle {
                width: parent.width
                height: parent.height
                y: -(1 - popup.popupProgress) * height
                color: Theme.bg

                ColumnLayout {
                    anchors.fill: parent
                    anchors.margins: 12
                    spacing: 8

                    RowLayout {
                        Layout.fillWidth: true
                        spacing: 6

                        Repeater {
                            model: root.interfaces
                            Rectangle {
                                id: tabBg
                                property bool active: root.selectedIface === modelData
                                property bool hovered: tabArea.containsMouse

                                color: active ? Qt.rgba(Theme.accent1.r, Theme.accent1.g, Theme.accent1.b, 0.2)
                                     : hovered ? Qt.rgba(Theme.fg.r, Theme.fg.g, Theme.fg.b, 0.1)
                                     : "transparent"
                                radius: 1000
                                implicitWidth: tabLabel.implicitWidth + 20
                                implicitHeight: tabLabel.implicitHeight + 8

                                Behavior on color {
                                    ColorAnimation { duration: 150; easing.type: Easing.OutCubic }
                                }

                                WrapperMouseArea {
                                    id: tabArea
                                    anchors.fill: parent
                                    cursorShape: Qt.PointingHandCursor
                                    hoverEnabled: true
                                    onClicked: root.selectIface(modelData)
                                }

                                Noto {
                                    id: tabLabel
                                    anchors.centerIn: parent
                                    font.pixelSize: 14
                                    color: tabBg.active ? Theme.accent1
                                         : tabBg.hovered ? Qt.lighter(Theme.fg, 1.2)
                                         : Theme.fg

                                    Behavior on color {
                                        ColorAnimation { duration: 150; easing.type: Easing.OutCubic }
                                    }
                                    text: modelData
                                }
                            }
                        }
                    }

                    RowLayout {
                        Layout.fillWidth: true
                        Noto {
                            font.pixelSize: 12
                            color: Theme.accent1
                            text: "\u2193 " + root.formatSpeed(root.currentRx)
                        }
                        Item { Layout.fillWidth: true }
                        Noto {
                            font.pixelSize: 12
                            color: Theme.accent2
                            text: "\u2191 " + root.formatSpeed(root.currentTx)
                        }
                    }

                    Canvas {
                        id: graph
                        Layout.fillWidth: true
                        Layout.fillHeight: true

                        onPaint: {
                            var ctx = getContext("2d")
                            var w = width
                            var h = height
                            ctx.clearRect(0, 0, w, h)

                            var d = root._data[root.selectedIface]
                            if (!d) return
                            var rxH = d.rxHistory
                            var txH = d.txHistory
                            if (rxH.length < 2) return

                            var maxVal = 1024
                            for (var i = 0; i < rxH.length; i++) {
                                maxVal = Math.max(maxVal, rxH[i], txH[i])
                            }
                            maxVal *= 1.1

                            ctx.strokeStyle = "rgba(255, 255, 255, 0.06)"
                            ctx.lineWidth = 1
                            for (var g = 1; g < 4; g++) {
                                var gy = Math.round(h * g / 4) + 0.5
                                ctx.beginPath()
                                ctx.moveTo(0, gy)
                                ctx.lineTo(w, gy)
                                ctx.stroke()
                            }

                            function drawSeries(history, color) {
                                var n = history.length
                                if (n < 2) return
                                var samples = root.maxSamples

                                var cr = Math.round(color.r * 255)
                                var cg = Math.round(color.g * 255)
                                var cb = Math.round(color.b * 255)

                                ctx.fillStyle = "rgba(" + cr + ", " + cg + ", " + cb + ", 0.12)"
                                ctx.beginPath()
                                var startX = ((samples - n) / (samples - 1)) * w
                                ctx.moveTo(startX, h)
                                for (var i = 0; i < n; i++) {
                                    var x = ((samples - n + i) / (samples - 1)) * w
                                    var y = h - (history[i] / maxVal) * h
                                    ctx.lineTo(x, y)
                                }
                                ctx.lineTo(w, h)
                                ctx.closePath()
                                ctx.fill()

                                ctx.strokeStyle = "rgba(" + cr + ", " + cg + ", " + cb + ", 1)"
                                ctx.lineWidth = 2
                                ctx.lineJoin = "round"
                                ctx.lineCap = "round"
                                ctx.beginPath()
                                for (var j = 0; j < n; j++) {
                                    var lx = ((samples - n + j) / (samples - 1)) * w
                                    var ly = h - (history[j] / maxVal) * h
                                    if (j === 0) ctx.moveTo(lx, ly)
                                    else ctx.lineTo(lx, ly)
                                }
                                ctx.stroke()
                            }

                            drawSeries(rxH, Theme.accent1)
                            drawSeries(txH, Theme.accent2)
                        }
                    }
                }
            }
        }
    }
}
