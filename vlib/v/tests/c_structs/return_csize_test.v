#include "@VMODROOT/csize.h"

struct C.Size {
	width  int
	height int
}

type Size = C.Size

fn get_size() C.Size {
	return C.Size{11, 22}
}

fn get_csize() Size {
	return Size(C.Size{11, 22})
}

fn test_return_csize() {
	println(get_size())
	assert true
	println(get_csize())
	assert true
}
