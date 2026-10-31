// Regression test for issue #28896. V reaches array data through pointers into
// the middle of a Boehm block (past the array header, and anywhere for slices),
// including from heap objects, so the runtime must have Boehm recognise interior
// pointers. The prebuilt libgc linked on Windows with tcc leaves that off, and a
// collection then freed blocks that live arrays and slices still used.
//
// This checks the setting itself: whether a particular block survives a
// collection depends on stale values in the conservatively scanned stack, which
// can keep a block alive by accident and make such a test pass without the fix.
fn C.GC_get_all_interior_pointers() int

fn test_boehm_recognises_interior_pointers() {
	$if gcboehm ? {
		assert C.GC_get_all_interior_pointers() == 1
	} $else {
		eprintln('skipping: not a Boehm GC build')
		assert true
	}
}
