module main

// Case pairs the two name spellings the generator derives from one proto name.
// The fields are not named `pascal`/`snake` on purpose: see the note in namer.v
// about `pascal` reaching the generated C verbatim.
struct Case {
	input string
	upper string
	snake string
}

const case_table = [
	Case{'get_request', 'GetRequest', 'get_request'}
	Case{'GetRequest', 'GetRequest', 'get_request'}
	Case{'get_request_', 'GetRequest', 'get_request_'}
	Case{'__foo', 'Foo', 'foo'}
	Case{'foo__bar', 'FooBar', 'foo_bar'}
	Case{'FOO', 'FOO', 'foo'}
	Case{'a', 'A', 'a'}
	Case{'one', 'One', 'one'}
	Case{'foo_bar_baz_', 'FooBarBaz', 'foo_bar_baz_'}
	Case{'', '', ''}
	Case{'_', '', ''}
	Case{'OneTwoThree', 'OneTwoThree', 'one_two_three'}
	Case{'HTTPServer', 'HTTPServer', 'httpserver'}
	Case{'aB', 'AB', 'a_b'}
	Case{'AB', 'AB', 'ab'}
	Case{'get2Value', 'Get2Value', 'get2value'}
	Case{'XMLHttpRequest', 'XMLHttpRequest', 'xmlhttp_request'},
]

fn test_pascal_case() {
	for c in case_table {
		assert pascal_case(c.input) == c.upper, 'pascal_case(${c.input})'
	}
}

fn test_snake_case() {
	for c in case_table {
		assert snake_case(c.input) == c.snake, 'snake_case(${c.input})'
	}
}

fn test_ascii_case_helpers() {
	assert to_upper(`a`) == `A`
	assert to_upper(`A`) == `A`
	assert to_upper(`0`) == `0`
	assert to_lower(`A`) == `a`
	assert to_lower(`a`) == `a`
	assert to_lower(`9`) == `9`
	assert is_upper_ascii(`A`)
	assert !is_upper_ascii(`a`)
	assert !is_upper_ascii(`1`)
	assert is_lower_ascii(`a`)
	assert !is_lower_ascii(`A`)
	assert !is_lower_ascii(`_`)
}

fn test_is_v_keyword() {
	// Only the exact reserved words are keywords: a suffixed or capitalised form
	// is not, which is what lets safe_field_name/safe_type_name escape by suffix.
	assert is_v_keyword('fn')
	assert is_v_keyword('mut')
	assert is_v_keyword('type')
	assert is_v_keyword('__global')
	assert !is_v_keyword('Type')
	assert !is_v_keyword('type_')
	assert !is_v_keyword('')
	assert !is_v_keyword('request')
}

fn test_safe_field_name() {
	assert safe_field_name('type') == 'type_'
	assert safe_field_name('mut') == 'mut_'
	assert safe_field_name('id') == 'id'
	// A field name keeps the schema's own casing; only keywords are suffixed.
	assert safe_field_name('userName') == 'userName'
	assert safe_field_name('Type') == 'Type'
}

fn test_safe_type_name() {
	assert safe_type_name('type') == 'Type'
	assert safe_type_name('get_request') == 'GetRequest'
	// The keyword check runs on the PascalCase form, so a lowercase keyword is
	// capitalised rather than escaped.
	assert safe_type_name('GetRequest') == 'GetRequest'
}

fn test_single_capital_name() {
	assert single_capital_name('M')
	assert !single_capital_name('m')
	assert !single_capital_name('MM')
	assert !single_capital_name('')
}

fn test_codec_function_names() {
	assert method_name('GetRequest') == 'get_request'
	assert decode_fn_name('GetRequest') == 'decode_get_request'
	assert decode_with_fn_name('GetRequest') == 'decode_get_request_with'
	assert read_fn_name('GetRequest') == 'read_get_request'
	assert map_encode_fn_name('KV', 'entries') == 'encode_kv_map_entries'
	assert map_entry_read_fn_name('KV', 'entries') == 'read_kv_map_entries_entry'
}

fn test_zero_value() {
	assert zero_value('int') == 'int(0)'
	assert zero_value('i32') == 'i32(0)'
	assert zero_value('u64') == 'u64(0)'
	assert zero_value('usize') == 'usize(0)'
	assert zero_value('rune') == 'rune(0)'
	assert zero_value('f32') == 'f32(0)'
	assert zero_value('f64') == 'f64(0)'
	assert zero_value('bool') == 'false'
	assert zero_value('string') == "''"
	// A composite type keeps its literal form.
	assert zero_value('Sub') == 'Sub{}'
	assert zero_value('map[string]int') == 'map[string]int{}'
}

fn test_join_package() {
	assert join_package(['google', 'rpc'], 'Status') == 'GoogleRpcStatus'
	assert join_package(['google'], 'Inner') == 'GoogleInner'
	// No package part means the type name alone is the flattened name.
	assert join_package([], 'Status') == 'Status'
}
