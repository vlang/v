struct Foo {
	x int
}

fn test_option_cast_eq_none() {
	assert ?int(none) == none
	assert !(?int(123) == none)
	assert ?int(123) != none

	assert ?string(none) == none
	assert ?string('hi') != none

	assert ?Foo(none) == none
	assert ?Foo(Foo{
		x: 1
	}) != none
}
