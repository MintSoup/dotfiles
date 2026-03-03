import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Services.Notifications
import Quickshell.Widgets

PanelWindow {
    anchors {
        top: true
        right: true
    }

    exclusionMode: ExclusionMode.Ignore
    color: "transparent"

    implicitWidth: 380
    implicitHeight: notifColumn.implicitHeight + margins.top

    margins.top: 36
    margins.right: 12

    NotificationServer {
        id: server
    }

    Column {
        id: notifColumn
        anchors {
            top: parent.top
            right: parent.right
            topMargin: 36
            rightMargin: 12
        }
        spacing: 8

        Repeater {
            model: server.trackedNotifications

            Rectangle {
                id: notif

                required property Notification modelData

                readonly property real timeout: modelData.expireTimeout > 0 ? modelData.expireTimeout : 5000

                width: 380
                height: notifLayout.implicitHeight + 24
                radius: 8
                color: Theme.bg

                Rectangle {
                    id: cooldownBar
                    anchors {
                        bottom: parent.bottom
                        left: parent.left
                    }
                    height: 2
                    radius: 8
                    color: Theme.active_ws

                    NumberAnimation on width {
                        from: notif.width
                        to: 0
                        duration: notif.timeout
                        running: true
                        onFinished: notif.modelData.dismiss()
                    }
                }

                RowLayout {
                    id: notifLayout
                    anchors {
                        top: parent.top
                        left: parent.left
                        right: parent.right
                        margins: 12
                        bottomMargin: 10
                    }
                    spacing: 10

                    IconImage {
                        source: modelData.appIcon !== ""
                            ? Quickshell.iconPath(modelData.appIcon, true)
                            : (modelData.image ?? "")
                        implicitSize: 32
                        mipmap: true
                        visible: source !== ""
                        Layout.alignment: Qt.AlignTop
                    }

                    ColumnLayout {
                        Layout.fillWidth: true
                        spacing: 2

                        Noto {
                            text: modelData.appName
                            font.pixelSize: Theme.fontSize - 2
                            color: Theme.active_ws
                            font.weight: Font.Medium
                            Layout.fillWidth: true
                        }

                        Noto {
                            text: modelData.summary
                            font.pixelSize: Theme.fontSize
                            color: Theme.fg
                            font.weight: Font.Bold
                            wrapMode: Text.WordWrap
                            Layout.fillWidth: true
                            visible: text !== ""
                        }

                        Noto {
                            text: modelData.body
                            font.pixelSize: Theme.fontSize - 1
                            color: Qt.rgba(Theme.fg.r, Theme.fg.g, Theme.fg.b, 0.75)
                            wrapMode: Text.WordWrap
                            Layout.fillWidth: true
                            visible: text !== ""
                        }
                    }

                    WrapperMouseArea {
                        Layout.alignment: Qt.AlignTop
                        onClicked: notif.modelData.dismiss()
                        Noto {
                            text: ""
                            font.pixelSize: Theme.fontSize - 2
                            color: Qt.rgba(Theme.fg.r, Theme.fg.g, Theme.fg.b, 0.5)
                        }
                    }
                }
            }
        }
    }
}
