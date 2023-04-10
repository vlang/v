import strings

fn myprintln[T](data T, mut str strings.Builder) T {
	$if T is $array {
		str.write_string('array: [')
		for i, elem in data {
			myprintln(elem, mut str)
			if i < data.len - 1 {
				str.write_string(', ')
			}
		}
		str.write_string(']')
	} $else $if T is $map {
		str.write_string('map: {')
		for key, val in data {
			str.write_string('(key) ')
			myprintln(key, mut str)
			str.write_string(' -> (value) ')
			myprintln(val, mut str)
		}
		str.write_string('}')
	} $else {
		str.write_string(data.str())
	}
	return data
}

struct Test {
mut:
	s strings.Builder
}

fn test_recursive_array() {
	mut t := Test{}
	myprintln([[1], [2], [3]], mut t.s)
	assert t.s.str() == 'array: [array: [1], array: [2], array: [3]]'
}

fn test_recursive_map() {
	mut t2 := Test{}
	myprintln({
		'a': [1, 2, 3]
		'b': [1000]
	}, mut t2.s)
	assert t2.s.str() == 'map: {(key) a -> (value) array: [1, 2, 3](key) b -> (value) array: [1000]}'
}
