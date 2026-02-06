module main

type OneD = []Thing

struct Thing {
	item int
}

fn good() [][]Thing {
	println('start good()')
	mut two_d := [][]Thing{}
	mut one_d := []Thing{}
	one_d << Thing{1}
	one_d << Thing{2}
	two_d << one_d
	assert two_d.len == 1, 'two_d length not 1'
	for i, one in two_d {
		for j, item in one {
			println('two_d[${i}][${j}]=${item}')
			assert item == Thing{j + 1}
		}
	}
	println('end good()')
	return two_d
}

fn bad() []OneD {
	println('start bad()')
	mut two_d := []OneD{}
	mut one_d := OneD{}
	one_d << Thing{1}
	one_d << Thing{2}
	assert one_d.len == 2, 'one_d length not 2'
	assert one_d[0] == Thing{1}, 'one_d[0] not 1'
	assert one_d[1] == Thing{2}, 'one_d[1] not 2'
	two_d << one_d
	assert two_d.len == 1, 'two_d length not 1'
	for i, one in two_d {
		for j, item in one {
			println('two_d[${i}][${j}]=${item}')
			assert item == Thing{j + 1}
		}
	}
	println('end bad()')
	return two_d
}

fn test_main() {
	good_two_d := good()
	assert good_two_d.len == 1, 'good_two_d length not 1'
	for i, one_d in good_two_d {
		for j, item in one_d {
			println('good_two_d[${i}][${j}]=${item}')
			assert item == Thing{j + 1}
		}
	}
	bad_two_d := bad()
	assert bad_two_d.len == 1, 'bad_two_d length not 1'
	for i, one_d in bad_two_d {
		for j, item in one_d {
			println('bad_two_d[${i}][${j}]=${item}')
			assert item == Thing{j + 1}
		}
	}
}
