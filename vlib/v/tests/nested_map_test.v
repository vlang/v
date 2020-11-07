struct Foo {
mut:
	name string
}

fn test_nested_maps() {
	if true {
	}
	//
	else {
	}
	mut x := map[string]map[string]int{}
	x['a'] = map[string]int{}
	assert x['a']['b'] == 0
	x['a']['b'] = 5
	assert x['a']['b'] == 5
	x['a']['b'] = 7
	assert x['a']['b'] == 7
	mut y := map[string]map[string]map[string]int{}
	y['a'] = map[string]map[string]int{}
	y['a']['b'] = map[string]int{}
	assert y['a']['b']['c'] == 0
	y['a']['b']['c'] = 5
	assert y['a']['b']['c'] == 5
	y['a']['b']['c'] = 7
	assert y['a']['b']['c'] == 7
	mut foos := map[string]map[string]Foo{}
	foos['a']['b'] = Foo{'bar'}
	assert foos['a']['b'].name == 'bar'
	foos['a']['b'].name = 'baz'
	assert foos['a']['b'].name == 'baz'
}
