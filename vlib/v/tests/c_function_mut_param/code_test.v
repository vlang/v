module main

#include "@VMODROOT/code.c"

fn C.mut_arg(key byteptr, mut val &size_t)

fn test_c_function_mut_param() {
	key := byteptr(1)
	mut val := size_t(1)
	C.mut_arg(key, mut &val)
	assert val == size_t(5)
}
