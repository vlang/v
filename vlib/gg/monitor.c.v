// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

import sokol.sapp

// MonitorInfo describes one display connected to the system.
pub struct MonitorInfo {
pub:
	index  int
	width  int
	height int
	name   string
}

// monitor_count returns 1: sokol.sapp only exposes the active window's
// framebuffer, not an OS monitor list. The multiwindow facade provides the
// full monitor enumeration when that build flag is enabled.
pub fn monitor_count() int {
	return 1
}

// monitor_info returns the active window's framebuffer size as the primary
// monitor. Index values other than 0 produce an empty value.
pub fn monitor_info(index int) MonitorInfo {
	if index != 0 {
		return MonitorInfo{ index: index }
	}
	return MonitorInfo{
		index:  0
		width:  sapp.width()
		height: sapp.height()
	}
}
