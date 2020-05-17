module clipboard

#include <libkern/OSAtomic.h>
#include <Cocoa/Cocoa.h>

#flag -framework Cocoa

pub struct Clipboard {
    pb voidptr
    last_cb_serial i64
mut:
	foo int // TODO remove, for mut hack
}

fn new_clipboard() &Clipboard{
	pb := voidptr(0)
	#pb = [NSPasteboard generalPasteboard];
	cb := &Clipboard{
		pb: pb
	}
	return cb
}

fn (cb &Clipboard) check_availability() bool {
	return cb.pb != C.NULL
}

fn (mut cb Clipboard) clear(){
	cb.foo = 0
	#[cb->pb clearContents];
}

fn (mut cb Clipboard) free(){
	cb.foo = 0
	//nothing to free
}

fn (cb &Clipboard) has_ownership() bool {
	if cb.last_cb_serial == 0 {return false}
	#return [cb->pb changeCount] == cb->last_cb_serial;
	return false
}

fn C.OSAtomicCompareAndSwapLong()

fn (mut cb Clipboard) set_text(text string) bool {
	cb.foo = 0
	#NSString *ns_clip;
	ret := false

	#ns_clip = [[ NSString alloc ] initWithBytesNoCopy:text.str length:text.len encoding:NSUTF8StringEncoding freeWhenDone: false];
	#[cb->pb declareTypes:[NSArray arrayWithObject:NSStringPboardType] owner:nil];
	#ret = [cb->pb setString:ns_clip forType:NSStringPboardType];
	#[ns_clip release];

	mut serial := 0
	#serial = [cb->pb changeCount];
	C.OSAtomicCompareAndSwapLong(cb.last_cb_serial, serial, &cb.last_cb_serial)
	return ret
}

fn (mut cb Clipboard) get_text() string {
	cb.foo = 0
	#NSString *ns_clip;
	utf8_clip := byteptr(0)

	#ns_clip = [cb->pb stringForType:NSStringPboardType]; //NSPasteboardTypeString
	#if (ns_clip == nil) {
	#	return tos3(""); //in case clipboard is empty
	#}

	#utf8_clip = [ns_clip UTF8String];
	return string(utf8_clip)
}

pub fn new_primary() &Clipboard {
	panic('Primary clipboard is not supported on non-Linux systems.')
}
