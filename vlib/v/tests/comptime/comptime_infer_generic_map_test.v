module main

fn encode_map[K, V](m map[K]V) string {
	return typeof(m).name
}

fn encode_array[T](arr []T) string {
	for element in arr {
		$if T is $map {
			return encode_map(element)
		}
	}
	return ''
}

fn test_main() {
	x := [{
		'123': '456'
	}, {
		'abc': 'def'
	}]
	assert encode_array(x) == 'map[string]string'
}
