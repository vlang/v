#include "@VMODROOT/vlib/v/tests/testdata/c_struct_tag_arg.h"

struct C.c_tag_point {
	x i32
	y i32
}

fn C.c_tag_point_sum(point &C.c_tag_point) i32

// `&C.c_tag_point(voidptr(p))` passed to a C function casts to the struct tag.
fn test_voidptr_cast_to_c_struct_tag_argument() {
	mut storage := [2]i32{}
	storage[0] = 20
	storage[1] = 22
	assert C.c_tag_point_sum(&C.c_tag_point(voidptr(&storage))) == 42
}
