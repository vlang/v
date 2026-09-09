module main

import x.multiwindow

#insert "@VMODROOT/vlib/x/multiwindow/native_render_result.h"
#insert "@VMODROOT/vlib/x/multiwindow/testdata/native_primitive_darwin_consumer.c"

fn C.v_multiwindow_test_darwin_primitive_size() usize
fn C.v_multiwindow_test_darwin_primitive_mask() u64

fn main() {
	assert C.v_multiwindow_test_darwin_primitive_size() == usize(104)
	assert C.v_multiwindow_test_darwin_primitive_mask() != 0
	capabilities := multiwindow.capabilities_for_backend(.appkit) or { panic(err) }
	assert capabilities.backend == .appkit
}
