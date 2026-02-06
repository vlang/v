module main

#include "@VMODROOT/code.c"

@[keep_args_alive]
fn C.foo(arg [1]int)

fn test_main() {
	C.foo([1]!)
}
