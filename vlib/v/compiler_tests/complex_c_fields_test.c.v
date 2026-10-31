module main

#include "@VMODROOT/vlib/v/compiler_tests/testdata/complex_c_fields.h"

struct C.VComplexFields {
mut:
	complex   int
	v_complex int
}

type ComplexFields = C.VComplexFields

fn test_c_complex_field_preserves_header_spelling() {
	mut fields := C.VComplexFields{
		complex:   41
		v_complex: 1
	}
	assert fields.complex + fields.v_complex == 42
	fields.complex++
	assert fields.complex == 42
	alias := ComplexFields{
		complex:   17
		v_complex: 3
	}
	pointer := &alias
	assert pointer.complex + pointer.v_complex == 20
}
