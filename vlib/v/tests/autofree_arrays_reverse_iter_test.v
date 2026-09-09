// vtest vflags: -autofree
// vtest build: !sanitize-address-gcc && !sanitize-address-clang
import arrays

fn test_arrays_fns_arg() {
	mut original := [10, 20]
	for mut x in arrays.reverse_iterator(original) {
		(*x)++
	}
	assert original == [11, 21]

	for mut x in original {
		(*x)++
	}
	assert original == [12, 22]
}
