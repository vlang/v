module clipboard

#include <libkern/OSAtomic.h>
#include <Cocoa/Cocoa.h>
#flag -framework Cocoa
#include "@VEXEROOT/vlib/clipboard/clipboard_darwin.m"

// Clipboard represents a system clipboard.
//
// System "copy" and "paste" actions utilize the clipboard for temporary storage.
pub struct Clipboard {
	pb             voidptr
	last_cb_serial i64
mut:
	foo int // TODO remove, for mut hack
}

fn C.darwin_new_pasteboard() voidptr

fn C.darwin_get_pasteboard_text(voidptr) &u8

fn C.darwin_set_pasteboard_text(voidptr, string) bool

fn new_clipboard() &Clipboard {
	cb := &Clipboard{
		pb: C.darwin_new_pasteboard() // pb
	}
	return cb
}

// check_availability returns true if the clipboard is ready to be used.
pub fn (cb &Clipboard) check_availability() bool {
	return cb.pb != C.NULL
}

// clear empties the clipboard contents.
pub fn (mut cb Clipboard) clear() {
	cb.foo = 0
	cb.set_text('')
	//#[cb->pb clearContents];
}

// free releases all memory associated with the clipboard
// instance.
pub fn (mut cb Clipboard) free() {
	cb.foo = 0
	// nothing to free
}

// has_ownership returns true if the contents of
// the clipboard were created by this clipboard instance.
pub fn (cb &Clipboard) has_ownership() bool {
	if cb.last_cb_serial == 0 {
		return false
	}
	//#return [cb->pb changeCount] == cb->last_cb_serial;
	return false
}

fn C.OSAtomicCompareAndSwapLong()

// set_text transfers `text` to the system clipboard.
// This is often associated with a *copy* action (`Cmd` + `C`).
pub fn (mut cb Clipboard) set_text(text string) bool {
	return C.darwin_set_pasteboard_text(cb.pb, text)
}

// get_text retrieves the contents of the system clipboard
// as a `string`.
// This is often associated with a *paste* action (`Cmd` + `V`).
pub fn (mut cb Clipboard) get_text() string {
	cb.foo = 0
	if isnil(cb.pb) {
		return ''
	}
	utf8_clip := C.darwin_get_pasteboard_text(cb.pb)
	return unsafe { tos_clone(&u8(utf8_clip)) }
}

// new_primary returns a new X11 `PRIMARY` type `Clipboard` instance allocated on the heap.
// Please note: new_primary only works on X11 based systems.
pub fn new_primary() &Clipboard {
	panic('Primary clipboard is not supported on non-Linux systems.')
}
