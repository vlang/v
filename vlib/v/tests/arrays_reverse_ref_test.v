module main

import arrays

fn test_main() {
	a := ['abc', 'def']

	for str in arrays.reverse_iterator(a) {
		println(str in a)
		assert (str in a) == true
	}
}
