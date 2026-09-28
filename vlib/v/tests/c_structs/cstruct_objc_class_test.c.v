$if macos {
	#flag -framework Cocoa
	#include <Cocoa/Cocoa.h>

	struct C.NSFont {}
}

fn test_native_class_keeps_its_header_declaration() {
	$if macos {
		font := unsafe { &C.NSFont(nil) }
		assert isnil(font)
	}
}
