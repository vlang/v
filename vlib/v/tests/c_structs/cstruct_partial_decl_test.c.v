#include "@VMODROOT/partial.h"

// The V-side declaration of a C struct is descriptive and may be partial or
// inexact: `bogus` does not exist in the real C struct. Init literals must not
// emit initializers for fields the user did not set (issue #27793).
struct C.PartialDecl {
	x     int
	y     i64
	bogus int
}

fn test_c_struct_init_literal_ignores_unset_fields() {
	a := C.PartialDecl{
		x: 42
	}
	b := C.PartialDecl{}
	assert a.x == 42
	assert a.y == 0
	assert b.x == 0
	assert b.y == 0
}
