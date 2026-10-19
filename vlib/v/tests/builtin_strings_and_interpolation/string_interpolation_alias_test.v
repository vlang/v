type Literal = string

fn test_array_alias_string() {
	a := [Literal('aaa'), Literal('bbb')]
	assert '${a}' == "['aaa', 'bbb']"
}

fn test_fixed_array_alias_string() {
	a := [Literal('aaa'), Literal('bbb')]!
	assert '${a}' == "['aaa', 'bbb']"
}

fn test_map_alias_string() {
	m := {
		'one': Literal('1')
		'two': Literal('2')
	}
	assert '${m}'.contains("'one': '1'")
	assert '${m}'.contains("'two': '2'")
}

type Duration = i64

fn test_i64_number_alias_string() {
	x := i64(9_123_456_789)
	y := Duration(x)
	assert '${x}' == '${y}'
}

struct Column {
	name string
}

type Columns = []Column

fn (c Columns) str() string {
	return c.map(it.name).join(', ')
}

struct Operation {
	columns []Column
}

fn (o &Operation) columns() Columns {
	return o.columns
}

fn table_columns() Columns {
	return [Column{'a'}, Column{'b'}]
}

fn test_interpolated_call_result_uses_the_str_method_of_its_alias() {
	op := &Operation{[Column{'x'}, Column{'y'}]}
	assert '(${op.columns()})' == '(x, y)'
	assert '(${table_columns()})' == '(a, b)'
}
