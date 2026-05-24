#include "@VMODROOT/cstruct.h"

struct C.MyRefStruct {
mut:
	format       &char
	name         &char
	children     &voidptr
	release      fn (&C.MyRefStruct)
	private_data voidptr
}

fn test_c_struct_zero_init_with_ref_fields() {
	s := C.MyRefStruct{}
	assert s.format == unsafe { nil }
	assert s.name == unsafe { nil }
	assert s.children == unsafe { nil }
	assert s.private_data == unsafe { nil }
}
