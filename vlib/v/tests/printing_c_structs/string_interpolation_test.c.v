#include "@VMODROOT/cstruct.h"

const the_string = 'the string'

pub struct C.Abc {
	char_pointer_field &char
}

struct VStruct {
	a_c_struct C.Abc
}

fn test_interpolation_of_v_structs_containing_c_structs() {
	abc := C.Abc{
		char_pointer_field: &char(the_string.str)
	}
	xxx := VStruct{
		a_c_struct: abc
	}
	sxxx := xxx.str()
	assert sxxx == 'VStruct{
    a_c_struct: C.Abc{
        char_pointer_field: &C"the string"
    }
}'
}

// Test for C structs where voidptr fields in V have different actual C types.
// This simulates the FreeType case where FT_Generic/FT_BBox are structs in C
// but may be declared as voidptr in V bindings.
pub struct C.OuterStruct {
	id    int
	inner voidptr // Declared as voidptr in V, but actual C type is struct InnerStruct
}

struct VStructWithCStruct {
	outer C.OuterStruct
}

fn test_c_struct_with_voidptr_field_that_is_actually_struct() {
	// This test verifies that printing a C struct with a voidptr field
	// that is actually a struct in C does not cause C compilation errors.
	outer := C.OuterStruct{
		id: 42
	}
	wrapper := VStructWithCStruct{
		outer: outer
	}
	s := wrapper.str()
	// The voidptr field should be printed as <cptr> placeholder
	assert s.contains('id: 42')
	assert s.contains('inner: <cptr>')
}
