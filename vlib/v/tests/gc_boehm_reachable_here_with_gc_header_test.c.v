// vtest vflags: -gc boehm
module main

// A module can include the collector header (fontstash does under `-gc boehm`) into
// the same unit as builtin's bodyless `C.GC_reachable_here` binding, `GC_noop1_ptr`.
// The prototype V emits for it must match the header's `volatile void *` parameter.
#include <gc.h>

fn keep_reachable(s string) string {
	doubled := s.repeat(2)
	$if gcboehm ? {
		C.GC_reachable_here(doubled.str)
	}
	return doubled
}

fn test_gc_reachable_here_with_the_collector_header() {
	assert keep_reachable('reachable') == 'reachablereachable'
}
