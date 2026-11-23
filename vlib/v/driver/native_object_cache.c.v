module driver

import os

#insert "@VEXEROOT/vlib/v/driver/native_object_cache.h"

fn C.v_native_object_cache_publish(const_source &char, const_destination &char) i32

// link_native_c_object keeps the platform error code and never replaces a
// published object, including when Android denies the hard-link operation.
fn link_native_c_object(temporary string, destination string) ! {
	$if windows {
		os.link(temporary, destination)!
	} $else {
		code := C.v_native_object_cache_publish(&char(temporary.str), &char(destination.str))
		if code != 0 {
			return error_with_code(os.get_error_msg(code), code)
		}
	}
}
