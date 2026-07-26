import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Io
import Quickshell.Hyprland
import Quickshell.Wayland

PanelWindow {
    id: root

    property string query: ""
    property bool shown: false
    property int selectedIndex: 0
    property var emojis: []
    readonly property int glyphSize: 40
    readonly property int cellSize: glyphSize + 44
    readonly property int columns: 8
    readonly property int rows: 6

    WlrLayershell.layer: WlrLayer.Overlay
    // Dedicated namespace so it gets its own Hyprland blur rule (higher
    // ignore_alpha) instead of the global "quickshell" one, which keeps the
    // blur painted almost to full transparency and leaves an empty blurred
    // rectangle visible during the fade-out.
    WlrLayershell.namespace: "emoji-picker"
    // Only grab the keyboard while open, so the always-mapped surface doesn't
    // hold focus when hidden.
    WlrLayershell.keyboardFocus: root.shown ? WlrKeyboardFocus.Exclusive : WlrKeyboardFocus.None

    GlobalShortcut {
        name: "emoji"
        description: "Open emoji picker"
        onPressed: root.shown = !root.shown
    }

    // No edge anchors: the compositor centers the surface on screen.
    exclusionMode: ExclusionMode.Ignore
    color: "transparent"

    implicitWidth: popup.implicitWidth
    implicitHeight: popup.implicitHeight

    // The surface stays permanently mapped and we animate purely in QML. This
    // avoids the per-open map/unmap cost and stops Hyprland from layering its
    // own fade animation on top of ours, which was the source of the stutter.
    visible: true

    // When hidden, expose no input region so clicks pass through the invisible
    // centered surface; when shown, capture input over the popup body.
    mask: Region { item: root.shown ? popup : null }

    // Clear search state after the fade-out completes, never mid-animation.
    onShownChanged: if (shown) resetTimer.stop(); else resetTimer.restart()
    Timer {
        id: resetTimer
        interval: 140
        onTriggered: {
            root.query = ""
            root.selectedIndex = 0
            searchInput.text = ""
        }
    }

    FileView {
        id: emojiFile
        path: Quickshell.shellDir + "/emoji-data.json"
        preload: true
        onLoaded: root.emojis = JSON.parse(emojiFile.text())
    }

    function close() {
        // Just trigger the fade-out; state is cleared in onVisibleChanged
        // once the surface is actually hidden.
        root.shown = false
    }

    // Copy the emoji to the clipboard and dismiss the picker.
    function pick(entry) {
        copyProc.command = ["wl-copy", "--", entry.e]
        copyProc.running = true
        root.close()
    }

    Process { id: copyProc }

    // Popup body
    Rectangle {
        id: popup
        anchors.centerIn: parent
        radius: 20
        color: Theme.bg
        border.width: 1
        border.color: Qt.rgba(1, 1, 1, 0.08)

        implicitWidth: content.implicitWidth + 48
        implicitHeight: content.implicitHeight + 48

        // Fade + subtle pop-in, both driven by QML on the always-mapped surface.
        opacity: root.shown ? 1 : 0
        scale: root.shown ? 1 : 0.96
        Behavior on opacity {
            NumberAnimation { duration: 120; easing.type: Easing.OutCubic }
        }
        Behavior on scale {
            NumberAnimation { duration: 120; easing.type: Easing.OutCubic }
        }

        ColumnLayout {
            id: content
            anchors.centerIn: parent
            spacing: 20

            // Search bar
            Rectangle {
                implicitHeight: 48
                Layout.fillWidth: true
                radius: 1000
                color: Qt.rgba(1, 1, 1, 0.08)

                TextInput {
                    anchors {
                        verticalCenter: parent.verticalCenter
                        left: parent.left
                        right: parent.right
                        leftMargin: 24
                        rightMargin: 24
                    }

                    id: searchInput
                    color: Theme.fg
                    font.family: Theme.font
                    font.pixelSize: Theme.fontSize * 1.25
                    focus: root.shown
                    clip: true
                    onTextChanged: {
                        root.query = text
                        root.selectedIndex = 0
                    }
                    Keys.onEscapePressed: root.close()
                    Keys.onUpPressed: root.selectedIndex = Math.max(0, root.selectedIndex - root.columns)
                    Keys.onDownPressed: root.selectedIndex = Math.min(grid.model.length - 1, root.selectedIndex + root.columns)
                    Keys.onLeftPressed: root.selectedIndex = Math.max(0, root.selectedIndex - 1)
                    Keys.onRightPressed: root.selectedIndex = Math.min(grid.model.length - 1, root.selectedIndex + 1)
                    Keys.onReturnPressed: {
                        if (grid.model[root.selectedIndex]) root.pick(grid.model[root.selectedIndex])
                    }

                    // Placeholder shown while empty.
                    Noto {
                        anchors.verticalCenter: parent.verticalCenter
                        text: "Search emoji…"
                        color: Qt.rgba(1, 1, 1, 0.35)
                        font.pixelSize: Theme.fontSize * 1.25
                        visible: searchInput.text.length === 0
                    }
                }
            }

            // Emoji grid
            GridView {
                id: grid
                Layout.alignment: Qt.AlignHCenter
                implicitWidth: root.cellSize * root.columns
                implicitHeight: root.cellSize * root.rows
                clip: true

                cellWidth: root.cellSize
                cellHeight: root.cellSize

                // Recycle delegates and keep a screenful cached so glyphs aren't
                // re-rasterized from scratch on every open / filter change.
                reuseItems: true
                cacheBuffer: root.cellSize * root.rows

                model: {
                    const q = root.query.toLowerCase().trim()
                    if (q === "") return root.emojis
                    return root.emojis.filter(e =>
                        e.n.includes(q) || e.s.includes(q) || e.g.toLowerCase().includes(q)
                    )
                }

                onModelChanged: root.selectedIndex = 0

                // Keep the highlighted cell scrolled into view.
                onCurrentIndexChanged: positionViewAtIndex(currentIndex, GridView.Contain)
                currentIndex: root.selectedIndex

                delegate: Rectangle {
                    id: cell
                    width: grid.cellWidth
                    height: grid.cellHeight
                    radius: 12

                    readonly property bool isSelected: index === root.selectedIndex
                    readonly property bool isHovered: hoverHandler.hovered

                    color: isSelected
                        ? Qt.rgba(1, 1, 1, 0.15)
                        : isHovered
                        ? Qt.rgba(1, 1, 1, 0.08)
                        : "transparent"

                    Behavior on color {
                        ColorAnimation { duration: 80 }
                    }

                    HoverHandler {
                        id: hoverHandler
                        onHoveredChanged: if (hovered) root.selectedIndex = index
                    }

                    MouseArea {
                        anchors.fill: parent
                        onClicked: root.pick(modelData)
                    }

                    Text {
                        anchors.centerIn: parent
                        text: modelData.e
                        font.family: "Noto Color Emoji"
                        font.pixelSize: root.glyphSize * 1.8
                    }
                }
            }

            // Name of the currently highlighted emoji.
            Noto {
                Layout.fillWidth: true
                horizontalAlignment: Text.AlignHCenter
                elide: Text.ElideRight
                font.pixelSize: Theme.fontSize * 1.1
                color: Qt.rgba(1, 1, 1, 0.7)
                text: grid.model[root.selectedIndex] ? grid.model[root.selectedIndex].n : ""
            }
        }
    }
}
