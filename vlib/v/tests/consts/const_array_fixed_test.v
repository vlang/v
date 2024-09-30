@[has_globals]
module main

struct Foo {
	bar int = 120
}

__global (
	a Foo
)

const c = [[a.bar]!]!

fn test_main() {
	dump(c == [[120]!]!)
}
