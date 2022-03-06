// Note: this test should be able to run without warnings/errors

type SumType = bool | int | string

fn test_reading_from_a_map_of_sumtype_values() {
	mut values := map[string]SumType{}
	values['abc'] = 'xyz'
	values['xyz'] = 123
	values['xxx'] = true
	println(unsafe { values['abcz'] }) // no warning/error, due to the unsafe{}
	if value := values['abc'] {
		eprintln('existing key `abc` is present, value: $value')
		assert true
	}
	if (values['abc'] or { 'zzzz' }) is string {
		eprintln('existing key `abc` is present, value is a string')
		assert true
	}
	if (values['abc'] or { 123 }) is int {
		eprintln('the existing keyed value is an int')
		assert false
	}
	if (values['something else'] or { 'zzzz' }) is string {
		eprintln('default value for non existing key is a string')
		assert true
	}
	if (values['something else'] or { 123 }) is int {
		eprintln('default value for non existing key is an int')
		assert true
	}
}
