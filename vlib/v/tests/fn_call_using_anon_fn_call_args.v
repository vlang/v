fn foofun1(op string) fn () string {
	return fn () string {
		return 'x passed'
	}
}

fn foofun2(op string) fn () int {
	return fn () int {
		return 22
	}
}

fn test_fn_call_using_anon_fn_call_arg() {
	println(main.foofun1('1')())
	assert main.foofun1('1')() == 'x passed'

	println(main.foofun2('1')())
	assert main.foofun2('1')() == 22
}
