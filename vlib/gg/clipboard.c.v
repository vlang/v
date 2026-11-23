// Clipboard helpers for gg, implemented on top of the sokol clipboard API.
//
// Note: sokol only enables clipboard support when the application sets
// `enable_clipboard: true` in its `sapp.Desc` (via gg's config). On backends
// where clipboard access is unavailable, sokol returns an empty string and
// writes are silently ignored.

module gg

import sokol.sapp

// set_clipboard_text copies `text` to the system clipboard.
//
// Clipboard access must have been enabled in the sokol/gg configuration for
// the write to take effect. V strings are NUL terminated, so `text.str` can be
// handed directly to sokol; no copy of the text is made here.
pub fn set_clipboard_text(text string) {
	sapp.set_clipboard_string(&u8(text.str))
}

// get_clipboard_text returns the current system clipboard contents as a
// freshly allocated V string.
//
// Clipboard access must have been enabled in the sokol/gg configuration. If
// sokol reports no clipboard content, an empty string is returned. The string
// is copied out of sokol's internal buffer, so it remains valid after the
// next clipboard event.
pub fn get_clipboard_text() string {
	ptr := sapp.get_clipboard_string()
	if ptr == unsafe { nil } {
		return ''
	}
	return unsafe { cstring_to_vstring(ptr) }
}
