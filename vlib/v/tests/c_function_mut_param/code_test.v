module main

#include "@VMODROOT/code.c"

fn C.mut_arg(key &byte, mut val &size_t)

fn test_c_function_mut_param() {
	key := &byte(1)
	mut val := size_t(1)
	C.mut_arg(key, mut &val)
	assert val == size_t(5)
}
