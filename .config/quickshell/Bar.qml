import Quickshell
import Quickshell.Hyprland
import Quickshell.Widgets
import QtQuick
import QtQuick.Layouts

Variants {
	model: Quickshell.screens;

	PanelWindow {
		color: Theme.bg

		anchors {
			top: true
			left: true
			right: true
		}

		implicitHeight: 32

		WrapperItem {
			id: rightWidgets
			anchors {
				right: parent.right
				verticalCenter: parent.verticalCenter
			}

			margin: 5

			RowLayout {
				spacing: 8

				Systray {}
				KbLayout {}
				Battery {}
				Temperature {}
				Memory {}
				Volume {}
				MicMute {}

				Noto {
					id: clock
					text: Time.time
				}
			}
		}

		WrapperItem {
			id: leftWidgets
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
							color: active ? Theme.accent1 : Theme.fg
						}
					}
				}
			}
		}
		Item {
			anchors {
				left: leftWidgets.right
				right: rightWidgets.left
				verticalCenter: parent.verticalCenter
			}

			Title {
				anchors.centerIn: parent
			}
		}
	}
}
