#include "@VMODROOT/csize.h"

struct C.Size {
	width  int
	height int
}

fn get_size() C.Size {
	return C.Size{11, 22}
}

fn test_return_csize() {
	println(get_size())
	assert true
}
