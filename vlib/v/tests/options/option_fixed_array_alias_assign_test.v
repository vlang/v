struct Box {}

type Arr = [2]Box

fn test_option_fixed_array_alias_assign_struct_init_literal() {
	mut a := ?Arr(none)

	a = Arr{}
	assert a != none
}

fn test_option_fixed_array_alias_assign_option_cast() {
	mut a := ?Arr(Arr{})

	a = ?Arr(none)
	assert a == none
}

fn test_option_fixed_array_alias_assign_plain_ident() {
	mut a := ?Arr(none)
	mut plain := Arr{}

	a = plain
	assert a != none
}

struct IndexCounter {
mut:
	calls int
}

fn next_index(mut counter IndexCounter) int {
	counter.calls++
	return 0
}

fn test_indexed_option_fixed_array_alias_assignment() {
	mut values := []?Arr{len: 1}
	mut counter := IndexCounter{}

	values[next_index(mut counter)] = Arr{}
	assert counter.calls == 1
	assert values[0] != none

	values[next_index(mut counter)] = ?Arr(none)
	assert counter.calls == 2
	assert values[0] == none
}
