const bit = f64(1)
const nibble = bit * 4
const bytes = bit * 8
const kb = bytes * 1000
const mb = kb * 1000

const units_map = {
	bit:    'bit'
	nibble: 'nibble'
	bytes:  'byte'
	kb:     'kB'
	mb:     'MB'
}

fn test_const_map_init_order() {
	println(units_map.len)
	println(units_map)
	assert units_map.len == 5
	assert units_map == {
		1.0:    'bit'
		4.0:    'nibble'
		8.0:    'byte'
		8000.0: 'kB'
		8.e+06: 'MB'
	}
}
