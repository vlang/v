fn test_map_of_map() {
	mut x := map[string]map[string]int{}
	x['a'] = map[string]int{}
	assert x['a']['b'] == 0
	x['a']['b'] = 5
	assert x['a']['b'] == 5
	x['a']['b'] = 7
	assert x['a']['b'] == 7
}

fn test_map_of_map_of_map() {
	mut y := map[string]map[string]map[string]int{}
	y['a'] = map[string]map[string]int{}
	y['a']['b'] = map[string]int{}
	assert y['a']['b']['c'] == 0
	y['a']['b']['c'] = 5
	assert y['a']['b']['c'] == 5
	y['a']['b']['c'] = 7
	assert y['a']['b']['c'] == 7
}

struct Foo {
mut:
	name string
}

fn test_map_of_map_to_struct() {
	mut foos := map[string]map[string]Foo{}
	foos['zza']['zzb'] = Foo{'bar'}
	assert foos['zza']['zzb'].name == 'bar'
	//
	foos['zza']['zzb'].name = 'baz'
	assert foos['zza']['zzb'].name == 'baz'
}
