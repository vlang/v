#flag -I @VEXEROOT/vlib/v/tests/pointers
#include "c_pointer_output_cast.h"

fn C.pointer_output_cast_probe(&voidptr)

fn test_c_pointer_output_cast_updates_caller_storage() {
	mut output := voidptr(unsafe { nil })
	C.pointer_output_cast_probe(voidptr(&output))
	assert !isnil(output)
	assert unsafe { *&i64(output) } == 37
}
