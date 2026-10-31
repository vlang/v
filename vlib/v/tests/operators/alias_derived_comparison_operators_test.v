// `>`, `<=` and `>=` are generated from the `<` that an alias declares,
// also when the aliased type is a struct or an enum, which have no `<`.
struct Point {
	x int
}

type Rank = Point

// < orders the ranks in reverse.
fn (a Rank) < (b Rank) bool {
	return a.x > b.x
}

enum Level {
	low = 1
	mid
	high
}

type Priority = Level

// < orders the priorities in reverse.
fn (a Priority) < (b Priority) bool {
	return int(Level(a)) > int(Level(b))
}

fn test_alias_of_struct_derives_comparisons_from_its_lt() {
	one := Rank(Point{1})
	two := Rank(Point{2})
	assert two < one
	assert one > two
	assert two <= one
	assert one >= two
	assert one <= one
	assert one >= one
	assert !(one < two)
	assert !(two > one)
	assert !(one <= two)
	assert !(two >= one)
}

fn test_alias_of_enum_derives_comparisons_from_its_lt() {
	low := Priority(Level.low)
	high := Priority(Level.high)
	assert high < low
	assert low > high
	assert high <= low
	assert low >= high
	assert low <= low
	assert !(low < high)
	assert !(high > low)
	assert !(low <= high)
	assert !(high >= low)
}
