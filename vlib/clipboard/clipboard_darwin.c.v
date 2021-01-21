module clipboard

#include <libkern/OSAtomic.h>
#include <Cocoa/Cocoa.h>
#flag -framework Cocoa
#include "@VROOT/vlib/clipboard/clipboard_darwin.m"
pub struct Clipboard {
	pb             voidptr
	last_cb_serial i64
mut:
	foo int // TODO remove, for mut hack
}

fn C.darwin_new_pasteboard() voidptr

fn C.darwin_get_pasteboard_text(voidptr) byteptr

fn C.darwin_set_pasteboard_text(string) bool

fn new_clipboard() &Clipboard {
	cb := &Clipboard{
		pb: C.darwin_new_pasteboard() // pb
	}
	return cb
}

fn (cb &Clipboard) check_availability() bool {
	return cb.pb != C.NULL
}

fn (mut cb Clipboard) clear() {
	cb.foo = 0
	cb.set_text('')
	//#[cb->pb clearContents];
}

fn (mut cb Clipboard) free() {
	cb.foo = 0
	// nothing to free
}

fn (cb &Clipboard) has_ownership() bool {
	if cb.last_cb_serial == 0 {
		return false
	}
	//#return [cb->pb changeCount] == cb->last_cb_serial;
	return false
}

fn C.OSAtomicCompareAndSwapLong()

fn (mut cb Clipboard) set_text(text string) bool {
	return C.darwin_set_pasteboard_text(cb.pb, text)
}

fn (mut cb Clipboard) get_text() string {
	cb.foo = 0
	if isnil(cb.pb) {
		return ''
	}
	utf8_clip := C.darwin_get_pasteboard_text(cb.pb)
	return unsafe { utf8_clip.vstring() }
}

// new_primary returns a new X11 `PRIMARY` type `Clipboard` instance allocated on the heap.
// Please note: new_primary only works on X11 based systems.
pub fn new_primary() &Clipboard {
	panic('Primary clipboard is not supported on non-Linux systems.')
}
