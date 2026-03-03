import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Services.Notifications
import Quickshell.Widgets

Scope {
	NotificationServer {
		id: server
		onNotification: n => { n.tracked = true }
	}

	PanelWindow {
		anchors {
			top: true
			right: true
		}

		exclusionMode: ExclusionMode.Ignore
		color: "transparent"
		implicitWidth: notifColumn.implicitWidth + 4
		implicitHeight: notifColumn.implicitHeight
		visible: server.trackedNotifications.values.length > 0

		margins.top: 40

		Column {
			id: notifColumn
			anchors {
				top: parent.top
				right: parent.right
				rightMargin: 8
			}
			spacing: 8

			Repeater {
				model: server.trackedNotifications

				Rectangle {
					id: notif

					required property Notification modelData

					readonly property real timeout: modelData.expireTimeout > 0 ? modelData.expireTimeout : 5000

					width: 380
					height: Math.min(notifLayout.implicitHeight + 24, 200)
					clip: true
					color: Theme.bg

					opacity: 0
					x: 200

					Component.onCompleted: {
						opacity = 1
						x = 0
					}

					Behavior on opacity { NumberAnimation { duration: 250; easing.type: Easing.OutCubic } }
					Behavior on x { NumberAnimation { duration: 250; easing.type: Easing.OutCubic } }

					function dismissWithAnim() {
						dismissAnim.start()
					}

					MouseArea {
						anchors.fill: parent
						onClicked: notif.dismissWithAnim()
					}

					SequentialAnimation {
						id: dismissAnim
						ParallelAnimation {
							NumberAnimation { target: notif; property: "opacity"; to: 0; duration: 200 }
							NumberAnimation { target: notif; property: "x"; to: 200; duration: 200; easing.type: Easing.InCubic }
						}
						ScriptAction { script: notif.modelData.dismiss() }
					}

					Rectangle {
						anchors {
							bottom: parent.bottom
							left: parent.left
						}
						height: 2
						color: Theme.accent2

						NumberAnimation on width {
							from: notif.width
							to: 0
							duration: notif.timeout
							running: true
							onFinished: notif.dismissWithAnim()
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

						Image {
							source: modelData.image ?? ""
							Layout.preferredWidth: 64
							Layout.preferredHeight: 64
							fillMode: Image.PreserveAspectCrop
							visible: source != ""
							clip: true
							Layout.alignment: Qt.AlignTop
						}

						ColumnLayout {
							Layout.fillWidth: true
							spacing: 2

							RowLayout {
								spacing: 4
								Layout.fillWidth: true

								IconImage {
									source: modelData.appIcon != ""
										? Quickshell.iconPath(modelData.appIcon, true)
										: ""
									implicitSize: 14
									mipmap: true
									visible: source != ""
								}

								Noto {
									text: modelData.appName
									font.pixelSize: Theme.fontSize - 2
									color: Theme.accent2
									font.weight: Font.Medium
									Layout.fillWidth: true
								}
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
					}
				}
			}
		}
	}
}
