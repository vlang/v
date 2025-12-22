module main

fn test_array_map_anon_return_fixed_array() {
	lines := ['1,2', '3,4', '5,7']
	coords := lines.map(fn (line string) [2]int {
		xs, ys := line.split_once(',') or { panic('invalid input') }
		x := xs.int()
		y := ys.int()
		return [x, y]!
	})

	assert coords == [[1, 2]!, [3, 4]!, [5, 7]!]
}
