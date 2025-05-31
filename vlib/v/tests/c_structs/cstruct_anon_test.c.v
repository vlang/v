#insert "@VMODROOT/anon.h"

@[typedef]
struct C.outer {
	inner struct {
		x int
	}
}

struct Outer {
	inner struct {
		val int
	}
}

fn test_main() {
	_ = Outer{}
	_ = C.outer{}
}
