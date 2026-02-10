import Quickshell
import Quickshell.Hyprland
import Quickshell.Widgets
import QtQuick
import QtQuick.Layouts

Variants {
	model: Quickshell.screens;

	PanelWindow {
		required property var modelData
		color: "#1e222a"
		/* property var backgroundColor: Qt.rgba(0, 0, 0, 0.2) */
		HyprlandWindow.opacity: 0.85

		anchors {
			top: true
			left: true
			right: true
		}

		implicitHeight: 30

		WrapperItem {
			anchors {
				right: parent.right
				verticalCenter: parent.verticalCenter
			}

			margin: 5

			RowLayout {
				Noto {
					id: clock
					text: Time.time
				}
			}
		}

		WrapperItem {
			anchors {
				left: parent.left
				verticalCenter: parent.verticalCenter
			}
			margin: 8

			RowLayout {
				spacing: 8

				Repeater {
					model: 9
					WrapperMouseArea {
						property int wsId: index + 1
						property var ws: Hyprland.workspaces.values.find(w => w.id === wsId)
						property bool active: Hyprland.focusedWorkspace?.id === wsId
						property bool hasWindows: ws && ws.toplevels.values.length > 0
						onClicked: Hyprland.dispatch("workspace " + wsId)
						visible: active || hasWindows

						Noto {
							text: {
								const icons = {
									"1": "",
									"2": "",
									"3": "",
									"4": "",
									"5": "",
									"6": "",
									"7": "",
									"8": " ",
									"9": "﫸 ",
								}
								icons[wsId]
							}
							color: active ? Theme.active_ws : Theme.fg
						}
					}
				}
			}

		}
	}
}
