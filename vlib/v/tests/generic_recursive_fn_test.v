fn myprintln[T](data T) T {
	$if T is $array {
		println('array: [')
		for i, elem in data {
			myprintln(elem)
			if i < data.len - 1 {
				print(', ')
			}
			println('')
		}
		println(']')
	} $else $if T is $map {
		println('map: {')
		for key, val in data {
			print('\t(key) ')
			myprintln(key)
			print(' -> (value) ')
			myprintln(val)
		}
		print('}')
	} $else {
		print(data)
	}
	return data
}

fn test_recursive_generic_fn() {
	assert myprintln([[1], [2], [3]]) == [[1], [2], [3]]
}
