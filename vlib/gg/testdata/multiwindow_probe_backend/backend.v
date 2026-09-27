module multiwindow_probe_backend

import gg
import os

pub const environment_name = 'V_MULTIWINDOW_PROBE_BACKEND'

pub fn selected() !gg.MultiWindowBackend {
	return match os.getenv(environment_name) {
		'' { error('multi-window probe backend must be selected explicitly') }
		'x11' { .x11 }
		'wayland' { .wayland }
		'appkit' { .appkit }
		'win32' { .win32 }
		else { error('multi-window probe backend must be x11, wayland, appkit, or win32') }
	}
}

pub fn validate(caps gg.Capabilities, expected gg.MultiWindowBackend) ! {
	if caps.backend != expected || !caps.native || !caps.multi_window || !caps.explicit_swapchain {
		return error('multi-window probe backend `${expected}` is unavailable: ${caps}')
	}
	matched := match expected {
		.x11 { caps.x11 && caps.gl }
		.wayland { caps.wayland && caps.gl }
		.appkit { caps.metal }
		.win32 { caps.win32 && caps.d3d11 }
		else { false }
	}

	if !matched {
		return error('multi-window probe backend `${expected}` reported incompatible capabilities: ${caps}')
	}
}
