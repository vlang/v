#include "@VMODROOT/cstruct.h"

struct C.MyCStruct {
	data &u8
}

struct MyWrapper {
	C.MyCStruct
}

fn (it C.MyCStruct) wrap() MyWrapper {
	return MyWrapper{
		data: it.data
	}
}

fn test_main() {
	dump(C.MyCStruct{
		data: &u8(123)
	}.wrap())
	assert true
}
