import maps

fn test_main() {
	// mmm map declaration as mutable
	mut mmm := map[string]map[string]int{}

	// adding values to the map mmm
	mmm['greet'] = {
		'hello': 0
	}
	mmm['place'] = {
		'world': 1
	}
	mmm['color']['orange'] = 2

	// printing the map mmm
	assert mmm.str() == "{'greet': {'hello': 0}, 'place': {'world': 1}, 'color': {'orange': 2}}"

	// mmm2 map declaration as const
	mmm2 := {
		'name': {
			'Diego': 3
		}
	}

	// printing the map mmm2
	assert mmm2.str() == "{'name': {'Diego': 3}}"

	// Using the maps module functions
	// use of maps.merge is commented but its behavior is the same as merge_in_place
	// mmm = maps.merge(mmm, mmm2)
	maps.merge_in_place(mut mmm, mmm2)

	// printing again mmm to the standard output
	assert mmm.str() == "{'greet': {'hello': 0}, 'place': {'world': 1}, 'color': {'orange': 2}, 'name': {'Diego': 3}}"
}
