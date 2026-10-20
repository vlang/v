#include <stdio.h>

// A system header in the program must not leave builtin's C.GC_stack_base an incomplete type.
fn test_gc_stack_base_is_complete_beside_a_system_header() {
	$if gcboehm ? {
		mut sb := C.GC_stack_base{}
		assert C.GC_get_stack_base(&sb) == 0
		assert sb.mem_base != unsafe { nil }
	}
}
