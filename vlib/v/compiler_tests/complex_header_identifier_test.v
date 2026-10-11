module main

#include <complex.h>

struct ComplexNamedField {
	complex int
}

fn complex_parameter(complex int) int {
	return complex + 1
}

fn test_complex_header_does_not_expand_v_identifiers() {
	complex := ComplexNamedField{
		complex: 41
	}
	assert complex.complex == 41
	assert complex_parameter(complex.complex) == 42
	assert '${complex.complex}' == '41'
}
