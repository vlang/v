// Test for issue #24471
// When returning a generic struct from a generic method with $if T is Type,
// the compiler should correctly resolve the function's generic parameter T
// instead of incorrectly using the struct init's concrete types.

module main

struct Type1 {
	item f32
}

struct Type2 {
	item int
}

struct GenericThing[T] {
	part1 T
	part2 T
}

struct AnotherGenericThing[T] {
	part3 T
}

// This used to fail with: `int` has no property `item`
// The bug was that inside `AnotherGenericThing[int]{...}`, the compiler
// incorrectly resolved T (the function's generic param) to `int` instead
// of keeping it as the receiver's actual type.
pub fn (thing GenericThing[T]) weird() AnotherGenericThing[int] {
	$if T is Type2 {
		return AnotherGenericThing[int]{thing.part1.item + thing.part2.item}
	} $else $if T is Type1 {
		return AnotherGenericThing[int]{int(thing.part1.item + thing.part2.item)}
	} $else {
		$compile_error('unrecognised type')
	}
}

fn test_generic_struct_init_with_comptime_if() {
	// Test with Type1 (f32 fields)
	thing1 := GenericThing{Type1{1.5}, Type1{2.5}}
	result1 := thing1.weird()
	assert result1.part3 == 4

	// Test with Type2 (int fields)
	thing2 := GenericThing{Type2{1}, Type2{2}}
	result2 := thing2.weird()
	assert result2.part3 == 3
}

// Additional test case: generic struct return with different type parameter
struct GenericResult[T] {
	val T
}

fn (thing GenericThing[T]) to_result() GenericResult[int] {
	$if T is Type2 {
		return GenericResult[int]{thing.part1.item + thing.part2.item}
	} $else $if T is Type1 {
		return GenericResult[int]{int(thing.part1.item + thing.part2.item)}
	} $else {
		return GenericResult[int]{0}
	}
}

fn test_generic_result_with_comptime_if() {
	g1 := GenericThing{Type1{3.0}, Type1{4.0}}
	r1 := g1.to_result()
	assert r1.val == 7

	g2 := GenericThing{Type2{5}, Type2{6}}
	r2 := g2.to_result()
	assert r2.val == 11
}
