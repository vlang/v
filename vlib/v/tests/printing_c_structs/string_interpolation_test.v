#include "@VMODROOT/cstruct.h"

const the_string = 'the string'

struct C.Abc {
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
    a_c_struct: struct Abc{
        char_pointer_field: &C"the string"
    }
}'
}
