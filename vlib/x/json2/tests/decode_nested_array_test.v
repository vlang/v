import x.json2

struct Dataset {
	values [][]string
}

struct Dataset2 {
	values [][]u8
}

fn test_string() {
	d := Dataset{
		values: [
			['a', 'b', 'c'],
			['d', 'e', 'f'],
		]
	}
	s := json2.encode(d)
	println(s)
	d2 := json2.decode[Dataset](s)!
	assert d2.str() == "Dataset{
    values: [['a', 'b', 'c'], ['d', 'e', 'f']]
}"
}

fn test_u8() {
	d := Dataset2{
		values: [
			[u8(1), 2, 3, 4, 5],
			[u8(2), 3, 4, 5, 6],
		]
	}
	s := json2.encode(d)
	println(s)
	d2 := json2.decode[Dataset2](s)!
	assert d2.str() == 'Dataset2{
    values: [[1, 2, 3, 4, 5], [2, 3, 4, 5, 6]]
}'
}
