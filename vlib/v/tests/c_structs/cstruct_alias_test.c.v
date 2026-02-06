#include "@VMODROOT/cstruct.h"

struct C.TestAlias {
}

type Foo = C.TestAlias

fn call() ?Foo {
	return Foo{}
}

fn test_main() {
	a := call()
	assert a?.str() == 'Foo(C.TestAlias{})'
}
