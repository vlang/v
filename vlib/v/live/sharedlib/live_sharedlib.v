module sharedlib

import v.live as _

pub fn set_live_reload_pointer(p voidptr) {
	eprintln('> set_live_reload_pointer, p: ${p}')
}
