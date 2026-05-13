import os as _

fn test_shadowed_c_fn_call_with_same_name_as_c_struct() {
	path := c'.'
	mut stat := C.stat{}
	unsafe {
		C.stat(path, &stat)
	}
}
