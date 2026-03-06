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

    WlrLayershell.keyboardFocus: WlrKeyboardFocus.Exclusive

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
    /* visible: shown */
    visible: HyprlandWindow.opacity > 0
    HyprlandWindow.opacity: shown ? 1 : 0

    Behavior on HyprlandWindow.opacity {
        NumberAnimation { duration: 100; easing.type: Easing.OutCubic }
    }

    function close() {
        root.shown = false
        root.query = ""
        root.selectedIndex = 0
        searchInput.text = ""
    }

    function launch(entry) {
        entry.execute()
        root.close()
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
			/* visible: false */

            cellWidth: root.cellSize
            cellHeight: root.cellSize

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
