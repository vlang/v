#include "@VMODROOT/cstruct.h"

const the_string = 'the string'

pub struct C.Abc {
	field int
}

fn (a &C.Abc) str() string {
	return 'C.Abc{}'
}

fn test_cstruct() {
	x := unsafe { &C.Abc(1) }
	println(x)
	assert dump(x.str()) == 'C.Abc{}'
}
