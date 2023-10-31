struct Bar {}

fn test_map_get_anon_fn_value_with_mut_arg() {
	foo := {
		0: fn (mut bar Bar) int {
			return 22
		}
	}

	mut bar := Bar{}
	ret := foo[0](mut bar)

	println(ret)
	assert ret == 22
}
