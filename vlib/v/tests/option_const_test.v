const var = get_var()

fn get_var() ?string {
	return none
}

fn test_main() {
	a := var
	assert dump(a) == 'default'
}
