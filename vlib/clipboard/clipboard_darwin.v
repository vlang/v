module clipboard

#include <libkern/OSAtomic.h>
#include <Cocoa/Cocoa.h>

#flag -framework Cocoa

struct Clipboard {
    pb voidptr
    last_cb_serial i64
}

fn new_clipboard() &Clipboard{
	mut pb := voidptr(0)
	#pb = [NSPasteboard generalPasteboard];
	cb := &Clipboard{
		pb: pb
	}
	return cb
}

fn (cb &Clipboard) check_availability() bool {
	return cb.pb != C.NULL
}

fn (cb &Clipboard) clear(){
	#[cb->pb clearContents];
}

fn (cb &Clipboard) free(){
	//nothing to free
}

fn (cb &Clipboard) has_ownership() bool {
	if cb.last_cb_serial == 0 {return false}
	#return [cb->pb changeCount] == cb->last_cb_serial;
	return false
}

fn C.OSAtomicCompareAndSwapLong()

fn (cb &Clipboard) set_text(text string) bool {
	#NSString *ns_clip;
	mut ret := false

	#ns_clip = [[ NSString alloc ] initWithBytesNoCopy:text.str length:text.len encoding:NSUTF8StringEncoding freeWhenDone: false];
	#[cb->pb declareTypes:[NSArray arrayWithObject:NSStringPboardType] owner:nil];
	#ret = [cb->pb setString:ns_clip forType:NSStringPboardType];
	#[ns_clip release];

	mut serial := 0
	#serial = [cb->pb changeCount];
	C.OSAtomicCompareAndSwapLong(cb.last_cb_serial, serial, &cb.last_cb_serial)
	return ret
}

fn (cb &Clipboard) get_text() string {
	#NSString *ns_clip;
	mut utf8_clip := byteptr(0)

	#ns_clip = [cb->pb stringForType:NSStringPboardType]; //NSPasteboardTypeString
	#if (ns_clip == nil) {
	#	return tos3(""); //in case clipboard is empty
	#}

	#utf8_clip = [ns_clip UTF8String];
	return string(utf8_clip)
}
