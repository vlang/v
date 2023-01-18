[gen_map]
enum Color {
	@none
	red
	green
	blue
}

fn test_as_map() {
	$for method in Color.methods {
		$if method.name == 'as_map' {
			assert true
			return
		}
	}
	assert false
}

fn test_as_map_contains() {
	assert 'none' in Color.@none.as_map()
	assert 'green' in Color.@none.as_map()
	assert 'blue' in Color.@none.as_map()
	assert 'blue' in Color.@none.as_map()
}

fn test_as_map_values() {
	assert Color.@none.as_map()['none'] == 0
	assert Color.@none.as_map()['red'] == 1
	assert Color.@none.as_map()['green'] == 2
	assert Color.@none.as_map()['blue'] == 3
}