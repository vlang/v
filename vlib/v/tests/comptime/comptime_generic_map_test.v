module main

fn validate[T](values map[string]T, rules map[string][]string) ! {
	for key, _ in rules {
		value := values[key]!
		assert typeof(value).name == T.name
	}
}

fn test_main() {
	validate({
		'age': 31
	}, {
		'age': [
			'required',
		]
	}) or { assert false }

	validate({
		'foo': 'bar'
	}, {
		'foo': [
			'required',
		]
	}) or { assert false }
	assert true
}
