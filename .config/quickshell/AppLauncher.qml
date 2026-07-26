import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Widgets
import Quickshell.Hyprland
import Quickshell.Wayland

PanelWindow {
    id: root

    property string query: ""
    property bool shown: false
    property int selectedIndex: 0
    readonly property int iconSize: 128
    readonly property int cellSize: iconSize + 64

    // Dedicated namespace so the always-mapped surface gets a blur rule with a
    // higher ignore_alpha, instead of the global "quickshell" one that keeps
    // blurring down to near-zero alpha and leaves a blurred screen during fade.
    WlrLayershell.namespace: "app-launcher"
    // Only hold the keyboard while open; the surface stays mapped otherwise.
    WlrLayershell.keyboardFocus: root.shown ? WlrKeyboardFocus.Exclusive : WlrKeyboardFocus.None

    GlobalShortcut {
        name: "launcher"
        description: "Open app launcher"
        onPressed: root.shown = !root.shown
    }

    anchors {
        top: true
        bottom: true
        left: true
        right: true
    }

    exclusionMode: ExclusionMode.Ignore
    color: "transparent"

    // Keep the surface permanently mapped and animate in QML. Avoids the cold
    // first-frame render (rasterizing all the 128px app icons at once) that
    // stutters the open animation on slower GPUs.
    visible: true

    // Expose no input region when hidden so the fullscreen surface doesn't
    // swallow clicks across the whole screen; capture everything when open.
    mask: Region { item: root.shown ? contents : null }

    // Clear search state after the fade-out finishes, never mid-animation.
    onShownChanged: if (shown) resetTimer.stop(); else resetTimer.restart()
    Timer {
        id: resetTimer
        interval: 100
        onTriggered: {
            root.query = ""
            root.selectedIndex = 0
            searchInput.text = ""
        }
    }

    function close() {
        // Just start the fade-out; state is cleared by resetTimer once hidden.
        root.shown = false
    }

    function launch(entry) {
        entry.execute()
        root.close()
    }

    Item {
        id: contents
        anchors.fill: parent

        opacity: root.shown ? 1 : 0
        Behavior on opacity {
            NumberAnimation { duration: 140; easing.type: Easing.OutCubic }
        }

        Rectangle {
            anchors.fill: parent
            color: "#88000000"
        }

        ColumnLayout {
            anchors.centerIn: parent
            width: parent.width * 0.8
            spacing: 32

            // Search bar
            Rectangle {
                implicitHeight: 48
                Layout.alignment: Qt.AlignHCenter
                implicitWidth: parent.width * 0.5
                radius: 1000
                color: Qt.rgba(1, 1, 1, 0.08)

                TextInput {
                    anchors {
                        verticalCenter: parent.verticalCenter
                        left: parent.left
                        leftMargin: 24
                    }

                    id: searchInput
                    color: Theme.fg
                    font.family: Theme.font
                    font.pixelSize: Theme.fontSize * 1.25
                    focus: root.shown
                    onTextChanged: {
                        root.query = text
                        root.selectedIndex = 0
                    }
                    Keys.onEscapePressed: root.close()
                    Keys.onUpPressed: root.selectedIndex = Math.max(0, root.selectedIndex - Math.floor(grid.width / root.cellSize))
                    Keys.onDownPressed: root.selectedIndex = Math.min(grid.model.length - 1, root.selectedIndex + Math.floor(grid.width / root.cellSize))
                    Keys.onLeftPressed: root.selectedIndex = Math.max(0, root.selectedIndex - 1)
                    Keys.onRightPressed: root.selectedIndex = Math.min(grid.model.length - 1, root.selectedIndex + 1)
                    Keys.onReturnPressed: {
                        if (grid.model[root.selectedIndex]) root.launch(grid.model[root.selectedIndex])
                    }
                }


            }

            // App grid
            GridView {
                id: grid
                Layout.alignment: Qt.AlignHCenter
                implicitHeight: root.cellSize * 4
                implicitWidth: Math.floor(parent.width / root.cellSize) * root.cellSize
                clip: true

                cellWidth: root.cellSize
                cellHeight: root.cellSize

                // Recycle delegates and keep a screenful cached so icons aren't
                // re-rasterized from scratch on every open / filter change.
                reuseItems: true
                cacheBuffer: root.cellSize * 4

                model: DesktopEntries.applications.values.filter(e =>
                    e.name.toLowerCase().includes(root.query.toLowerCase())
                )

                onModelChanged: root.selectedIndex = 0

                delegate: Rectangle {
                    id: cell
                    width: grid.cellWidth
                    height: grid.cellHeight

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
                        onClicked: root.launch(modelData)
                    }

                    ColumnLayout {
                        anchors.centerIn: parent
                        spacing: 4

                        IconImage {
                            source: Quickshell.iconPath(modelData.icon ?? "", true)
                            implicitSize: root.iconSize
                            mipmap: true
                            visible: source != ""
                            Layout.alignment: Qt.AlignHCenter
                        }

                        Noto {
                            text: modelData.name
                            Layout.alignment: Qt.AlignHCenter
                            elide: Text.ElideRight
                            Layout.maximumWidth: root.cellSize - 8
                        }
                    }
                }
            }
        }
    }
}
