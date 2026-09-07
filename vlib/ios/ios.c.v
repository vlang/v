module ios

import macos

#flag -framework UIKit
#insert "@VEXEROOT/vlib/ios/uikit_bridge.h"

fn C.ios_ui_application_main(argc int, argv &&char, principal voidptr, delegate voidptr) int
fn C.ios_color_from_hex(hex u32) voidptr
fn C.ios_color_from_hex_alpha(hex u32, alpha f64) voidptr
fn C.ios_index_path_row(index_path voidptr) i64
fn C.ios_index_path_for_row(row i64, section i64) voidptr

// Start the UIKit application run loop.
@[inline]
pub fn application_main(argc int, argv &&char, delegate_class_name string) int {
	return C.ios_ui_application_main(argc, argv, unsafe { nil },
		macos.nsstring(delegate_class_name))
}

// Create a UIColor from a hex value (0xRRGGBB).
@[inline]
pub fn color(hex u32) macos.Id {
	return C.ios_color_from_hex(hex)
}

// Create a UIColor from a hex value with alpha.
@[inline]
pub fn color_alpha(hex u32, alpha f64) macos.Id {
	return C.ios_color_from_hex_alpha(hex, alpha)
}

// Get the row index from an NSIndexPath.
@[inline]
pub fn index_path_row(index_path macos.Id) int {
	return int(C.ios_index_path_row(index_path))
}

// Create an NSIndexPath for a row in a section.
@[inline]
pub fn index_path(row int, section int) macos.Id {
	return C.ios_index_path_for_row(i64(row), i64(section))
}
