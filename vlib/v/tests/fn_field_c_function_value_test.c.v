#include <stdlib.h>

fn C.abs(x int) int

struct CFunctionValueHolder {
	cb fn (int) int = unsafe { nil }
}

fn c_function_value_apply(cb fn (int) int, value int) int {
	return cb(value)
}

fn test_c_function_values_keep_their_signature() {
	direct := C.abs
	assert direct(-3) == 3
	assert c_function_value_apply(C.abs, -4) == 4
	holder := CFunctionValueHolder{
		cb: C.abs
	}
	assert holder.cb(-5) == 5
}

fn test_receiver_named_like_its_c_function_value() {
	abs := CFunctionValueHolder{
		cb: C.abs
	}
	assert abs.cb(-6) == 6
	assert c_function_value_apply(abs.cb, -7) == 7
}
