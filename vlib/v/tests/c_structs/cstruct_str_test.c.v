#include "@VMODROOT/cstruct.h"

@[typedef]
struct C.Test2 {}

@[typedef]
struct C.Test1 {
	a C.Test2
}

fn (s C.Test2) str() string {
	return 'test2'
}

fn (s C.Test1) get() C.Test1 {
	return s
}

fn (s C.Test1) s() string {
	return '.${s.get()}.'
}

type TestAlias = C.Test2

fn (s TestAlias) str() string {
	return 'test_alias'
}

fn (s TestAlias) get() TestAlias {
	return s
}

fn test_main() {
	x := unsafe { &C.Test1(malloc(1024)) }
	println(x)
	assert dump('${x}') == '&C.Test1{
    a: test2
}'
	println('.${x.get()}.${x.s()}')

	y := unsafe { &TestAlias(malloc(1024)) }
	println(y)
	assert dump('${y}') == '&test_alias'
	println('.${y.get()}.')

	w := TestAlias(*y)
	assert dump(w.str()) == 'test_alias'

	assert true
}
