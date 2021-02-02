module clipboard

import os

// new returns a new `Clipboard` instance allocated on the heap.
// The `Clipboard` resources can be released with `free()`
pub fn new() &Clipboard {
	return new_clipboard()
}

// copy copies `text` into the clipboard.
pub fn (mut cb Clipboard) copy(text string) bool {
	// WayLand workaround using wl-clipboard.
	if os.getenv('WAYLAND_DISPLAY') != '' {
		if os.exists_in_system_path('wl-copy') && os.exists_in_system_path('wl-paste') {
			success := os.system('wl-copy $text') == 0
			cb.is_owner = success
			return success
		} else {
			return false
		}
	}
	// Termux clipboard support
	if os.exists_in_system_path('termux-clipboard-set') && os.exists_in_system_path('termux-clipboard-get') {
		success := os.system('termux-clipboard-set $text') == 0
		cb.is_owner = success
		return success
	}

	return cb.set_text(text)
}

// paste returns current entry as a `string` from the clipboard.
pub fn (mut cb Clipboard) paste() string {
	// WayLand workaround using wl-clipboard.
	if os.getenv('WAYLAND_DISPLAY') != '' {
		if os.exists_in_system_path('wl-copy') && os.exists_in_system_path('wl-paste') {
			result := os.exec('wl-paste --no-newline') or {
				return ''
			}
			return result.output
		}
	} else if os.exists_in_system_path('termux-clipboard-set') && os.exists_in_system_path('termux-clipboard-get') {
			// Termux clipboard support
			result := os.exec('termux-clipboard-get') or {
				return ''
			}
			return result.output
	} 
	return cb.get_text()
}

// clear_all clears the clipboard.
pub fn (mut cb Clipboard) clear_all() {
	// wayland and termux implementation have different ways to clear clipboard
	// check for them before
	if os.getenv('WAYLAND_DISPLAY') != '' {
		if os.exists_in_system_path('wl-copy') && os.exists_in_system_path('wl-paste') {
			 os.system('wl-copy -c')
			 cb.is_owner = false
		}
	} else if os.exists_in_system_path('termux-clipboard-set') && os.exists_in_system_path('termux-clipboard-get') {
		os.system('termux-clipboard-set  ""')
		cb.is_owner = false
	} else {
		cb.clear()
	}
}

// destroy destroys the clipboard and free it's resources.
pub fn (mut cb Clipboard) destroy() {
	// nothing to destroy in case of wayland and termux
	if os.getenv('WAYLAND_DISPLAY') != '' || os.exists_in_system_path('termux-clipboard-set') { return }
	cb.free()
}

// check_ownership returns `true` if the `Clipboard` has the content ownership.
pub fn (cb Clipboard) check_ownership() bool {
	return cb.has_ownership()
}

// is_available returns `true` if the clipboard is available for use.
pub fn (cb &Clipboard) is_available() bool {
	if os.getenv('WAYLAND_DISPLAY') != '' {
		return (os.exists_in_system_path('wl-copy') && os.exists_in_system_path('wl-paste'))
	}
	if os.exists_in_system_path('termux-clipboard-set') { return true }

	return cb.check_availability()
}
