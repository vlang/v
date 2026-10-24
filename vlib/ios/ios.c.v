module ios

import macos

#flag -framework UIKit

// UIApplicationMain is a C function in UIKit.
fn C.UIApplicationMain(argc int, argv &&char, principal_class_name voidptr, delegate_class_name voidptr) int

// Start the UIKit application run loop.
@[inline]
pub fn application_main(argc int, argv &&char, delegate_class_name string) int {
	return C.UIApplicationMain(argc, argv, unsafe { nil }, macos.nsstring(delegate_class_name))
}

// Create a UIColor from a hex value (0xRRGGBB).
@[inline]
pub fn color(hex u32) macos.Id {
	r := f64((hex >> 16) & 0xff) / 255.0
	g := f64((hex >> 8) & 0xff) / 255.0
	b := f64(hex & 0xff) / 255.0
	return macos.msg_id_four_f64(macos.get_class('UIColor'), 'colorWithRed:green:blue:alpha:', r, g, b, 1.0)
}

// Create a UIColor from a hex value with alpha.
@[inline]
pub fn color_alpha(hex u32, alpha f64) macos.Id {
	r := f64((hex >> 16) & 0xff) / 255.0
	g := f64((hex >> 8) & 0xff) / 255.0
	b := f64(hex & 0xff) / 255.0
	return macos.msg_id_four_f64(macos.get_class('UIColor'), 'colorWithRed:green:blue:alpha:', r, g, b, alpha)
}

// Get the row index from an NSIndexPath.
@[inline]
pub fn index_path_row(index_path macos.Id) int {
	return int(macos.msg_i64(index_path, 'row'))
}

// Create an NSIndexPath for a row in a section.
@[inline]
pub fn index_path(row int, section int) macos.Id {
	return macos.msg_id2(macos.get_class('NSIndexPath'), 'indexPathForRow:inSection:', unsafe { macos.Id(row) }, unsafe { macos.Id(section) })
}
