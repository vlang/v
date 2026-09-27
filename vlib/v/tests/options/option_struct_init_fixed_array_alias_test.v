struct Box {}

type Arr = [2]Box

fn test_option_struct_init_fixed_array_alias() {
	a := ?Arr{}
	assert a == none

	b := ?Arr(none)
	assert b == none
}
