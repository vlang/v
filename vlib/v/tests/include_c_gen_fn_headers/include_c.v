#flag -I.
#include "cheader.h"

// fn C.hello_from_c()

// fn C.add_two_numbers(a int, b int) int

fn test_include_c_autogen_of_fn_definitions() {
	C.hello_from_c()
	x := C.add_two_numbers(2, 3)
	assert x == 5
}

fn main() {
	test_include_c_autogen_of_fn_definitions()
}
