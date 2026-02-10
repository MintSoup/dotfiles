pragma Singleton

import Quickshell
import QtQuick

Singleton {
	id: root
	readonly property string time: {
		// TODO: move formatting into the widget itself
		Qt.formatDateTime(clock.date, " ddd, d MMM yyyy   hh:mm:ss")
	}

	SystemClock {
		id: clock
		precision: SystemClock.Seconds
	}
}
