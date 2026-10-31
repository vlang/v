module clipboard

import macos

#flag darwin -framework Cocoa

@[heap]
pub struct Clipboard {
	pb             voidptr
	last_cb_serial i64
mut:
	foo int // TODO: remove, for mut hack
}

fn new_clipboard() &Clipboard {
	pb := macos.msg_id(macos.get_class('NSPasteboard'), 'generalPasteboard')
	return &Clipboard{
		pb: pb
	}
}

@[unsafe]
pub fn (cb &Clipboard) check_availability() bool {
	return cb.pb != unsafe { nil }
}

pub fn (mut cb Clipboard) clear() {
	cb.foo = 0
	cb.set_text('')
}

pub fn (mut cb Clipboard) free() {
	cb.foo = 0
	// nothing to free
}

pub fn (cb &Clipboard) has_ownership() bool {
	if cb.last_cb_serial == 0 {
		return false
	}
	return false
}

@[unsafe]
pub fn (mut cb Clipboard) set_text(text string) bool {
	if cb.pb == unsafe { nil } {
		return false
	}
	ns_clip := macos.nsstring(text)
	pasteboard_type := macos.nsstring('public.utf8-plain-text')
	macos.msg_void2(cb.pb, 'declareTypes:owner:', macos.msg_id1(macos.get_class('NSArray'), 'arrayWithObject:', pasteboard_type), unsafe { nil })
	macos.msg_void2(cb.pb, 'setString:forType:', ns_clip, pasteboard_type)
	return true
}

// new_primary returns a new X11 `PRIMARY` type `Clipboard` instance allocated on the heap.
// Please note: new_primary only works on X11 based systems.
pub fn new_primary() &Clipboard {
	panic('Primary clipboard is not supported on non-Linux systems.')
}

@[unsafe]
pub fn (mut cb Clipboard) get_text() string {
	cb.foo = 0
	if cb.pb == unsafe { nil } {
		return ''
	}
	pasteboard_type := macos.nsstring('public.utf8-plain-text')
	ns_clip := macos.msg_id1(cb.pb, 'stringForType:', pasteboard_type)
	if ns_clip == unsafe { nil } {
		return ''
	}
	return macos.utf8_string(ns_clip)
}
