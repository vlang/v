#insert "@VMODROOT/anon.h"

@[typedef]
struct C.outer {
	inner struct {
		x int
	}
}

fn test_main() {
	a := C.outer{}
	b := C.outer{}
	assert a == b
}
