module main

#include "@VMODROOT/code.c"

fn C.mut_arg(key &byte, mut val usize)

fn test_c_function_mut_param() {
	key := &byte(1)
	mut val := usize(1)
	C.mut_arg(key, mut &val)
	assert val == usize(5)
}
