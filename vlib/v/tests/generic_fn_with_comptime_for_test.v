fn fn_a<T>(data T, depth int, nl bool) {
	for _ in 0 .. depth {
		print('\t')
	}
	$if T.typ is int {
		print('int: $data')
	} $else $if T.typ is string {
		print('string: $data')
	} $else $if T is $Array {
		println('array: [')
		for i, elem in data {
			fn_a(elem, depth + 1, false)
			if i < data.len - 1 {
				print(', ')
			}
			println('')
		}
		print(']')
	} $else $if T is $Map {
		println('map: {')
		for key, value in data {
			print('\t(key) ')
			fn_a(key, depth, false)
			print(' -> (value) ')
			fn_a(value, depth, true)
		}
		print('}')
	} $else $if T is $Struct {
		println('struct $T.name: {')
		$for field in T.fields {
			print('\t($field.name) ')
			fn_a(data.$(field.name), depth, true)
			// uncommenting either of these lines will cause a C error in my branch as the type is
			// set manually to the $for field type, it needs to be fixed to infer the type correctly.
			fn_a(['1', '2', '3', '4'], depth + 1, true)
			fn_a(0.111, depth + 1, true)
			fn_a('hello', depth + 1, true)
		}
		print('}')
	}
	if nl {
		println('')
	}
}

struct StructA {
	field_a int
	field_b string
}

fn test_generic_fn_with_comptime_for() {
	fn_a(111, 0, true)
	fn_a('hello', 0, true)
	fn_a(['a', 'b', 'c', 'd'], 0, true)
	fn_a({
		'one': 1
		'two': 2
	}, 0, true)
	fn_a(StructA{ field_a: 111, field_b: 'vlang' }, 0, true)
	assert true
}
