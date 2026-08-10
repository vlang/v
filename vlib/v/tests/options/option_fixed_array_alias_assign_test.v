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
