module main

#include "@DIR/posix_check.h"
#flag @DIR/posix_check.c

fn C.v_freestanding_posix_check() i32

fn main() {
	checked := C.v_freestanding_posix_check()
	assert checked == 0, checked.str()
	println('POSIX IO ok')
}
