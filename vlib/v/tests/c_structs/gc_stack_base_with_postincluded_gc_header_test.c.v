// vtest vflags: -gc boehm
#include <stdio.h>
#postinclude <gc.h>

// gc.h postincluded, after the V type block, defines struct GC_stack_base too late for the
// GC_H guard: builtin's layout must not be emitted for such a program.
fn test_a_postincluded_gc_h_is_not_given_a_second_gc_stack_base() {
	assert true
}
