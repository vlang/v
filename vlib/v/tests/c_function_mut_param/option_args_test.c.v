module main

#include "@VMODROOT/code.c"

fn C.mut_arg(&u8, mut val uint)

fn test_c_function_mut_param() {
	key := unsafe { &u8(1) }
	mut val := uint(1)
	C.mut_arg(key, mut &val)
	assert val == uint(5)
}
