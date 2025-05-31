module main

fn func(arg ?string, val &int) ?int {
	unsafe {
		*val = 2
	}
	return 2
}

fn test_main() {
	map1 := {
		'json': func
	}
	assert typeof(map1['json']).name == 'fn (?string, int) ?int'

	number := 0
	ret := map1['json']('hi', &number)
	assert number == ret?
	assert ret? == 2

	mut map2 := map[string]fn (?string, &int) ?int{}
	map2['json'] = func
	assert typeof(map2['json']).name == 'fn (?string, int) ?int'

	number2 := 0
	ret2 := map2['json']('', &number2)
	assert number2 == ret2?
	assert ret2? == 2
}
