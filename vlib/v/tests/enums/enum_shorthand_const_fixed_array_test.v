enum Other {
	a
	b
}

enum Kind {
	x
	y
	b
}

type KindAlias = Kind

// an enum shorthand in a const fixed array resolves against the array's
// element type, not against whichever enum in the program happens to
// have a value with that name
const fixed = [Kind.x, .b]!
const dynamic = [Kind.x, .b]
const alias_fixed = [KindAlias(Kind.x), .b]!
const other_fixed = [Other.a, .b]!

fn test_main() {
	assert fixed == [Kind.x, Kind.b]
	assert dynamic == [Kind.x, Kind.b]
	assert Kind.b in fixed
	assert alias_fixed == [KindAlias(Kind.x), KindAlias(Kind.b)]
	assert other_fixed == [Other.a, Other.b]
	_ = Other.a
}
