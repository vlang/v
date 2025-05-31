fn func(arg string) ?int {
	return 2
}

fn func2(arg string) int {
	return 2
}

fn func3(arg string) fn (string) int {
	return func2
}

fn func4(arg string) fn (string) ?int {
	return func
}

fn test_main() {
	map1 := {
		'f': func
	}
	ret := map1['f']('test')?
	assert ret == 2

	map2 := {
		'f': func2
	}
	ret2 := map2['f']('test')
	assert ret2 == 2

	map3 := {
		'f': func3
	}
	assert map3['f']('test')('test') == 2

	map4 := {
		'f': func4
	}
	assert map4['f']('test')('test')? == 2
}
