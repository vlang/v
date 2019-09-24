module darwin

#include <Cocoa/Cocoa.h>
#flag -framework Cocoa

struct C.NSString { }

// macOS and iOS helpers
pub fn nsstring(s string) *C.NSString {
	// println('ns $s len=$s.len')
	# return [ [ NSString alloc ] initWithBytesNoCopy:s.str  length:s.len
	# encoding:NSUTF8StringEncoding freeWhenDone: false];
	return 0
	
	//ns := C.alloc_NSString()
	//return ns.initWithBytesNoCopy(s.str, length: s.len,
		//encoding: NSUTF8StringEncoding,		freeWhenDone: false)
}

