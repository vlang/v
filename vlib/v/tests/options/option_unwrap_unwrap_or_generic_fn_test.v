fn unwrap[T](t ?T) T {
	return t or { panic('none') }
}

fn unwrap_or[T](t ?T, t_or T) T {
	return t or { t_or }
}

struct S1 {
	value int
}

struct S2 {}

fn test_unwrap() {
	s1 := ?S1{
		value: 42
	}
	assert unwrap(s1).value == 42

	// see #25566
	// s2 := ?S2(S2{})
	// assert unwrap(s2) == S2{}
}

fn test_unwrap_or() {
	s1 := ?S1{
		value: 42
	}
	assert unwrap_or(s1, S1{ value: 12 }).value == 42

	s1n := ?S1(none)
	assert unwrap_or(s1n, S1{ value: 12 }).value == 12

	s2 := ?S2{}
	assert unwrap_or(s2, S2{}) == S2{}
}
