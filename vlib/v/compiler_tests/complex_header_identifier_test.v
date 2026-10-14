module main

#include <complex.h>

struct ComplexNamedField {
	complex   int
	v_complex int
}

fn complex_parameter(complex int, v_complex int) int {
	return complex + v_complex
}

fn test_complex_header_does_not_expand_v_identifiers() {
	complex := ComplexNamedField{
		complex:   41
		v_complex: 1
	}
	v_complex := 1
	assert complex.complex == 41
	assert complex.v_complex == v_complex
	assert complex_parameter(complex.complex, v_complex) == 42
	assert '${complex.complex}' == '41'
}
