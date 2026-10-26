// vtest vflags: -gc boehm
#include <gc.h>

// A program that includes gc.h itself gets gc.h's definition of C.GC_stack_base, not a second
// one from builtin. (Calling C.GC_get_stack_base here is a separate matter: builtin declares it
// with a `voidptr` parameter, which gc.h's prototype conflicts with.)
fn test_gc_stack_base_is_defined_once_beside_gc_h() {
	$if gcboehm ? {
		sb := C.GC_stack_base{}
		assert sizeof(C.GC_stack_base) >= sizeof(voidptr)
		assert sb.mem_base == unsafe { nil }
	}
}
