@[has_globals]
module main

struct Foo {
	bar int = 120
}

__global (
	a Foo
)

const c = [[a.bar]!]!
const d = [[a.bar]]!
const e = [[a.bar]!]
const f = [[[a.bar]!]!]!
const g = [a.bar]

fn test_selector() {
	assert dump(c == [[a.bar]!]!)
	assert dump(d == [[a.bar]]!)
	assert dump(e == [[a.bar]!])
	assert dump(f == [[[a.bar]!]!]!)
	assert dump(g == [a.bar])
}

fn test_literal() {
	assert dump(c == [[120]!]!)
	assert dump(d == [[120]]!)
	assert dump(e == [[120]!])
	assert dump(f == [[[120]!]!]!)
	assert dump(g == [120])
}
