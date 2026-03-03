import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Services.SystemTray
import Quickshell.Widgets

RowLayout {
    spacing: 4

    Repeater {
        model: SystemTray.items

        WrapperMouseArea {
            id: trayItem

            required property SystemTrayItem modelData

            acceptedButtons: Qt.LeftButton | Qt.RightButton

            onClicked: mouse => {
                if (mouse.button === Qt.RightButton) {
                    menuAnchor.open(modelData.menu)
                } else {
                    modelData.activate()
                }
            }

            QsMenuAnchor {
                id: menuAnchor
                anchor.window: trayItem.QsWindow.window
            }

            IconImage {
                source: modelData.icon
                implicitSize: Theme.fontSize * 1.25
            }
        }
    }
}
