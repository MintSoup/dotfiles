import QtQuick
import Quickshell.Io
import Quickshell.Widgets

WrapperItem {
    id: root

    property int temp: 0
    readonly property string icon: {
        if (temp < 50) return ""
        if (temp < 70) return ""
        if (temp < 90) return ""
        return ""
    }

    FileView {
        id: tempFile
        blockLoading: true
    }

    Timer {
        interval: 1000
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: {
            tempFile.reload()
            root.temp = parseInt(tempFile.text()) / 1000
        }
    }

    Noto {
        text: root.icon + " " + root.temp + "°C"
    }

	Component.onCompleted: {
		for (let i = 0; i < 20; i++) {
			const nameFile = `/sys/class/hwmon/hwmon${i}/name`
			const f = Qt.createQmlObject(`
            import Quickshell.Io
            FileView { path: "${nameFile}"; blockLoading: true }
        `, root)
			const name = f.text().trim()
			f.destroy()
			if (name === "k10temp" || name === "coretemp") {
				tempFile.path = `/sys/class/hwmon/hwmon${i}/temp1_input`
				break
			}
		}
	}
}
