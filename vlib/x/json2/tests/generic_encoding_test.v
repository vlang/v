import x.json2 as json

type StringAlias = string
type SumTypes = bool | int | string

struct StructType[T] {
mut:
	val T
}

struct StructTypeOption[T] {
mut:
	val ?T
}

struct StructTypePointer[T] {
mut:
	val &T
}

fn test_nested_generic_structs() {
	typed_string_struct := StructType[string]{
		val: 'a'
	}
	
	// StructType[StructType[string]]
	nested1 := StructType[StructType[string]]{
		val: typed_string_struct
	}
	result1 := json.encode(nested1)
	assert result1 == '{"val":{"val":"a"}}'
	
	// Test with prettify
	pretty1 := json.encode(nested1, prettify: true)
	assert pretty1.contains('"val":')
	assert pretty1.contains('"val": "a"')
}

fn test_generic_with_option() {
	typed_string_struct := StructType[string]{
		val: 'test'
	}
	
	// StructType[StructTypeOption[string]]
	// Note: StructTypeOption[string] expects ?string, but we're passing StructType[string]
	// This test may need adjustment based on actual behavior
	nested_option := StructTypeOption[string]{
		val: 'test'
	}
	result := json.encode(nested_option)
	assert result == '{"val":"test"}'
	
	// StructTypeOption[StructType[string]]
	option_nested := StructTypeOption[StructType[string]]{
		val: typed_string_struct
	}
	result2 := json.encode(option_nested)
	assert result2 == '{"val":{"val":"test"}}'
}

fn test_generic_with_alias() {
	typed_string_struct := StructType[StringAlias]{
		val: 'alias'
	}
	
	result := json.encode(typed_string_struct)
	assert result == '{"val":"alias"}'
	
	// Nested with alias
	nested_alias := StructType[StructType[StringAlias]]{
		val: typed_string_struct
	}
	result2 := json.encode(nested_alias)
	assert result2 == '{"val":{"val":"alias"}}'
}

fn test_generic_with_sumtype() {
	typed_sum := StructType[SumTypes]{
		val: 42
	}
	
	result_int := json.encode(typed_sum)
	assert result_int == '{"val":42}'
	
	typed_sum_str := StructType[SumTypes]{
		val: 'hello'
	}
	result_str := json.encode(typed_sum_str)
	assert result_str == '{"val":"hello"}'
	
	typed_sum_bool := StructType[SumTypes]{
		val: true
	}
	result_bool := json.encode(typed_sum_bool)
	assert result_bool == '{"val":true}'
}

fn test_nested_generic_combinations() {
	typed_string_struct := StructType[string]{
		val: 'base'
	}
	
	// StructType[StructType[StructType[string]]]
	triple_nested := StructType[StructType[StructType[string]]]{
		val: StructType[StructType[string]]{
			val: typed_string_struct
		}
	}
	result := json.encode(triple_nested)
	assert result == '{"val":{"val":{"val":"base"}}}'
	
	// Test with prettify
	pretty := json.encode(triple_nested, prettify: true)
	assert pretty.contains('"val":')
}

fn test_generic_with_arrays() {
	typed_array := StructType[[]int]{
		val: [1, 2, 3]
	}
	
	result := json.encode(typed_array)
	assert result == '{"val":[1,2,3]}'
	
	// Nested array in generic
	nested_array := StructType[StructType[[]string]]{
		val: StructType[[]string]{
			val: ['a', 'b']
		}
	}
	result2 := json.encode(nested_array)
	assert result2 == '{"val":{"val":["a","b"]}}'
}

fn test_generic_with_maps() {
	typed_map := StructType[map[string]int]{
		val: {
			'key1': 1
			'key2': 2
		}
	}
	
	result := json.encode(typed_map)
	assert result.contains('"val"')
	assert result.contains('"key1"')
	assert result.contains('1')
}

fn test_option_generic_combinations() {
	typed_string_struct := StructType[string]{
		val: 'test'
	}
	
	// StructTypeOption[StructTypeOption[string]]
	double_option := StructTypeOption[StructTypeOption[string]]{
		val: StructTypeOption[string]{
			val: 'inner'
		}
	}
	result := json.encode(double_option)
	assert result == '{"val":{"val":"inner"}}'
	
	// Option with none
	none_option := StructTypeOption[StructType[string]]{}
	result2 := json.encode(none_option)
	assert result2 == '{}'
}

fn main() {
	test_nested_generic_structs()
	println('✓ Nested generic structs')
	
	test_generic_with_option()
	println('✓ Generic with option')
	
	test_generic_with_alias()
	println('✓ Generic with alias')
	
	test_generic_with_sumtype()
	println('✓ Generic with sumtype')
	
	test_nested_generic_combinations()
	println('✓ Nested generic combinations')
	
	test_generic_with_arrays()
	println('✓ Generic with arrays')
	
	test_generic_with_maps()
	println('✓ Generic with maps')
	
	test_option_generic_combinations()
	println('✓ Option generic combinations')
	
	println('')
	println('All generic encoding tests passed!')
}
