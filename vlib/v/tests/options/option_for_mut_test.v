module main

fn test_main() {
	mut data := [3]?int{}

	for mut d in data {
		d = ?int(1)
		assert '${d}' == 'Option(1)'
	}

	for i in 0 .. data.len {
		data[i] = ?int(3)
	}
	assert '${data}' == '[Option(3), Option(3), Option(3)]'
}
