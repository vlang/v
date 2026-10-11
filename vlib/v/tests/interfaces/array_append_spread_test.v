interface Value {}

type Codes = []string

type FixedCodes = [2]string

struct ValueHolder {
mut:
	values ?[]Value
}

fn test_array_append_spread_boxes_each_element() {
	codes := ['IT', 'US']
	mut params := []Value{}
	params << codes
	assert params.len == 1
	assert '${params}' == "[Value(['IT', 'US'])]"

	params = []Value{}
	params << ...codes
	assert params.len == 2
	assert '${params}' == "[Value('IT'), Value('US')]"
	assert params[0] is string
	assert params[1] is string
}

fn test_array_append_spread_literal_and_empty_array() {
	mut params := []Value{}
	params << ...['IT', 'US']
	assert '${params}' == "[Value('IT'), Value('US')]"
	params << ...[]string{}
	assert params.len == 2
}

fn test_array_append_spread_array_alias() {
	codes := Codes(['IT', 'US'])
	mut params := []Value{}
	params << ...codes
	assert '${params}' == "[Value('IT'), Value('US')]"
}

fn test_array_append_spread_fixed_array_slice() {
	codes := ['IT', 'US']!
	more := ['DE']!
	mut params := []Value{}
	params << ...codes[..]
	params << ...more[..]
	assert params.len == 3
	assert '${params}' == "[Value('IT'), Value('US'), Value('DE')]"
	assert params[0] is string
}

fn test_array_append_spread_fixed_array_alias_slice() {
	codes := FixedCodes(['IT', 'US']!)
	mut params := []Value{}
	params << ...codes[..]
	assert '${params}' == "[Value('IT'), Value('US')]"
}

fn test_array_append_spread_scalar_uses_single_value_lowering() {
	count := 8
	mut params := []Value{}
	params << ...7
	params << ...count
	params << ...'IT'
	assert params.len == 3
	assert '${params}' == "[Value(7), Value(8), Value('IT')]"
	assert params[0] is int
	assert params[0] == 7
	assert params[1] is int
	assert params[1] == count
	assert params[2] is string
}

fn test_array_append_spread_nested_arrays() {
	codes := [['IT'], ['US']]
	mut params := []Value{}
	params << ...codes
	assert params.len == 2
	assert '${params}' == "[Value(['IT']), Value(['US'])]"
	assert params[0] is []string
}

fn test_array_append_spread_existing_interface_values() {
	codes := [Value('IT'), Value('US')]
	mut params := []Value{}
	params << ...codes
	assert '${params}' == "[Value('IT'), Value('US')]"
}

fn test_array_append_spread_optional_target() {
	codes := ['IT', 'US']
	mut holder := ValueHolder{
		values: []Value{}
	}
	holder.values? << ...codes
	params := holder.values or { panic('missing values') }
	assert '${params}' == "[Value('IT'), Value('US')]"
}

fn spread_codes(mut calls []int) []string {
	calls << 1
	return ['IT', 'US']
}

fn test_array_append_spread_evaluates_source_once() {
	mut calls := []int{}
	mut params := []Value{}
	params << ...(spread_codes(mut calls))
	assert calls == [1]
	assert '${params}' == "[Value('IT'), Value('US')]"
}

interface NamedValue {
	name() string
}

struct Country {
	code string
}

fn (country Country) name() string {
	return country.code
}

fn test_array_append_spread_preserves_interface_methods() {
	codes := [Country{'IT'}, Country{'US'}]
	mut params := []NamedValue{}
	params << ...codes
	assert params.len == 2
	assert params[0].name() == 'IT'
	assert params[1].name() == 'US'
}

fn spread_values[T](values []T) []Value {
	mut params := []Value{}
	params << ...values
	return params
}

fn test_array_append_spread_generic_source() {
	assert '${spread_values(['IT', 'US'])}' == "[Value('IT'), Value('US')]"
	params := spread_values([7, 8])
	assert params.len == 2
	assert params[0] is int
	assert params[1] is int
	assert params[0] == 7
	assert params[1] == 8
}

fn test_array_append_spread_integer_values() {
	mut params := []Value{}
	params << ...[7, 8]
	assert '${params}' == '[Value(7), Value(8)]'
}
