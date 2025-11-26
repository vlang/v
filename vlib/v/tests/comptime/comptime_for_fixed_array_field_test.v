struct StructWithFixedArrays {
	number u64
	bytes  [8]u8
	floats [4]f32
	name   string
	matrix [2][3]int
}

fn collect_field_info[T](t T) []string {
	mut info := []string{}
	$for f in T.fields {
		v := t.$(f.name)
		info << '${f.name}: ${v}'
	}
	return info
}

fn count_accessible_fields[T](t T) int {
	mut count := 0
	$for f in T.fields {
		_ := t.$(f.name)
		count++
	}
	return count
}

fn test_fixed_array_field_access() {
	test := StructWithFixedArrays{
		number: 42
		bytes:  [u8(1), 2, 3, 4, 5, 6, 7, 8]!
		floats: [f32(1.0), 2.0, 3.0, 4.0]!
		name:   'test'
		matrix: [[1, 2, 3]!, [4, 5, 6]!]!
	}

	info := collect_field_info(test)

	assert info.len == 5
	assert info[0].contains('number: 42')
	assert info[1].contains('bytes: [1, 2, 3, 4, 5, 6, 7, 8]')
	assert info[2].contains('floats: [1.0, 2.0, 3.0, 4.0]')
	assert info[3].contains('name: test')
	assert info[4].contains('matrix: [[1, 2, 3], [4, 5, 6]]')
}

fn test_nested_fixed_arrays() {
	test := StructWithFixedArrays{
		number: 100
		bytes:  [u8(10), 20, 30, 40, 50, 60, 70, 80]!
		floats: [f32(0.1), 0.2, 0.3, 0.4]!
		name:   'nested'
		matrix: [[10, 20, 30]!, [40, 50, 60]!]!
	}

	assert count_accessible_fields(test) == 5
}

fn test_fixed_array_comptime_iteration() {
	test := StructWithFixedArrays{
		number: 999
		bytes:  [u8(0), 1, 2, 3, 4, 5, 6, 7]!
		floats: [f32(1.1), 2.2, 3.3, 4.4]!
		name:   'iteration'
		matrix: [[100, 200, 300]!, [400, 500, 600]!]!
	}

	mut field_names := []string{}
	mut field_values := []string{}

	$for f in StructWithFixedArrays.fields {
		field_names << f.name
		val := test.$(f.name)
		field_values << '${val}'
	}
	assert field_names.len == 5
	assert field_values.len == 5

	assert 'number' in field_names
	assert 'bytes' in field_names
	assert 'floats' in field_names
	assert 'name' in field_names
	assert 'matrix' in field_names

	assert field_values[0].contains('999')
	assert field_values[3].contains('iteration')
}
