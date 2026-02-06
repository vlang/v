#insert "@VMODROOT/foo.h"

struct C.foo {
	a int
}

fn ref[T](x T) &T {
	return &x
}

fn test_main() {
	a := C.foo{}
	b := ref(a)
}
