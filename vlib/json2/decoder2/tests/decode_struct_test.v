import json2.decoder2 as json
import time

const fixed_time = time.new(
	year:   2022
	month:  3
	day:    11
	hour:   13
	minute: 54
	second: 25
)

type StringAlias = string
type BoolAlias = bool
type IntAlias = int
type TimeAlias = time.Time
type StructAlias = StructType[int]
type EnumAlias = Enumerates

type SumTypes = StructType[string] | []SumTypes | []string | bool | string | time.Time | u32

enum Enumerates {
	a
	b
	c
	d
	e = 99
	f
}

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

struct StructWithID {
	id int
}

struct StructWithNumber {
	n f64
}

struct NestedChild {
	id int
}

struct StructWithNestedChild {
	child NestedChild
	name  string
}

struct StructWithJsonAliases {
	first_name string @[json: firstName]
	last_name  string @[json: 'lastName']
}

struct StructWithSkippedField {
	name   string
	secret int @[skip]
}

struct StructWithCollectionDefaults {
	xs []int          = [9]
	m  map[string]int = {
		'old': 1
	}
}

struct StructWithRequiredFields {
	name                string  @[required]
	skip_if_present     ?string @[required; skip]
	not_required_number int
}

struct StructWithRawFields {
	id      string  @[raw]
	object  string  @[raw]
	payload ?string @[raw]
}

fn test_types() {
	assert json.decode[StructType[string]]('{"val": ""}')!.val == ''

	assert json.decode[StructType[string]]('{"val": "2"}')!.val == '2'

	assert json.decode[StructType[int]]('{"val": 2}')!.val == 2

	assert json.decode[StructType[map[string]string]]('{"val": {"val1": "test"}}')!.val['val1'] == 'test'

	assert json.decode[StructType[Enumerates]]('{"val": 0}')!.val == Enumerates.a
	assert json.decode[StructType[Enumerates]]('{"val": 1}')!.val == Enumerates.b

	assert json.decode[StructType[StringAlias]]('{"val": "2"}')!.val == StringAlias('2')
	assert json.decode[StructType[BoolAlias]]('{"val": true}')!.val == BoolAlias(true)
	assert json.decode[StructType[IntAlias]]('{"val": 2}')!.val == IntAlias(2)
	assert json.decode[StructType[TimeAlias]]('{"val": "2022-03-11T13:54:25.000Z"}')!.val == TimeAlias(fixed_time)
	assert json.decode[StructType[TimeAlias]]('{"val": "2022-03-11T13:54:25.000Z"}')!.val.unix() == fixed_time.unix()
	assert json.decode[StructType[StructAlias]]('{"val": {"val": 2}}')!.val == StructAlias(StructType[int]{
		val: 2
	})
	assert json.decode[StructAlias]('{"val": 2}')!.val == 2
	assert json.decode[StructType[EnumAlias]]('{"val": 0}')!.val == EnumAlias(Enumerates.a)
	assert json.decode[StructType[EnumAlias]]('{"val": 1}')!.val == EnumAlias(Enumerates.b)

	assert json.decode[StructType[time.Time]]('{"val": "2022-03-11T13:54:25.000Z"}')!.val == fixed_time
	assert json.decode[StructType[time.Time]]('{"val": "2022-03-11T13:54:25.000Z"}')!.val.unix() == fixed_time.unix()
}

fn test_option_types() {
	if x := json.decode[StructTypeOption[string]]('{}')!.val {
		assert false, 'Should return none'
	} else {
		assert err.msg() == ''
		assert true
	}

	if x := json.decode[StructTypeOption[string]]('{"val": "2"}')!.val {
		assert x == '2'
	} else {
		assert false, 'Should not return none'
	}

	if x := json.decode[StructTypeOption[int]]('{"val": null}')!.val {
		assert false, 'Should return none, got ${x}'
	}

	if x := json.decode[StructTypeOption[int]]('{"val": 0}')!.val {
		assert x == 0
	} else {
		assert false, 'Should preserve a present zero value'
	}
}

fn test_unknown_nested_struct_values_are_skipped() {
	assert json.decode[StructWithID]('{"ignored": {"id": 1}}')!.id == 0
	assert json.decode[StructWithID]('{"ignored": [1, 2], "id": 3}')!.id == 3
}

fn test_struct_field_with_exponent_notation() {
	assert json.decode[StructWithNumber]('{"n": 1e3}')!.n == 1000.0
}

fn test_invalid_time_strings_return_errors() {
	mut failed := false
	json.decode[time.Time]('"not-a-date"') or { failed = true }
	assert failed

	failed = false
	json.decode[StructType[time.Time]]('{"val": "not-a-date"}') or { failed = true }
	assert failed
}

fn test_nested_struct_does_not_skip_next_sibling() {
	parent := json.decode[StructWithNestedChild]('{"child":{"id":1},"name":"bob"}')!
	assert parent == StructWithNestedChild{
		child: NestedChild{
			id: 1
		}
		name:  'bob'
	}

	children := json.decode[[]NestedChild]('[{"id":1},{"id":2}]')!
	assert children == [NestedChild{
		id: 1
	}, NestedChild{
		id: 2
	}]
}

fn test_struct_json_field_aliases() {
	user := json.decode[StructWithJsonAliases]('{"firstName":"Ada","lastName":"Lovelace"}')!
	assert user == StructWithJsonAliases{
		first_name: 'Ada'
		last_name:  'Lovelace'
	}

	escaped_key := json.decode[StructWithJsonAliases](r'{"first\u004eame":"Grace"}')!
	assert escaped_key.first_name == 'Grace'
}

fn test_struct_skip_field() {
	with_numeric_value := json.decode[StructWithSkippedField]('{"name":"Ada","secret":42}')!
	assert with_numeric_value == StructWithSkippedField{
		name: 'Ada'
	}

	with_wrong_value_kind :=
		json.decode[StructWithSkippedField]('{"name":"Ada","secret":"ignored"}')!
	assert with_wrong_value_kind.secret == 0
}

fn test_present_collections_replace_struct_defaults() {
	decoded := json.decode[StructWithCollectionDefaults]('{"xs":[1],"m":{"new":2}}')!
	assert decoded.xs == [1]
	assert decoded.m == {
		'new': 2
	}

	empty := json.decode[StructWithCollectionDefaults]('{"xs":[],"m":{}}')!
	assert empty.xs == []
	assert empty.m == {}
}

fn test_absent_collections_preserve_struct_defaults() {
	decoded := json.decode[StructWithCollectionDefaults]('{}')!
	assert decoded.xs == [9]
	assert decoded.m == {
		'old': 1
	}
}

fn test_required_struct_fields() {
	decoded := json.decode[StructWithRequiredFields]('{"name":"Ada","skip_if_present":42}')!
	assert decoded == StructWithRequiredFields{
		name:            'Ada'
		skip_if_present: none
	}

	for input in ['{}', '{"name":"Ada"}', '{"skip_if_present":42}'] {
		mut failed := false
		json.decode[StructWithRequiredFields](input) or { failed = true }
		assert failed, 'Expected `${input}` to fail because a required field is missing'
	}
}

fn test_raw_struct_fields_preserve_json_source() {
	decoded :=
		json.decode[StructWithRawFields]('{"id":1,"object":{"a":[1,true]},"payload":[null,2]}')!
	assert decoded.id == '1'
	assert decoded.object == '{"a":[1,true]}'
	assert decoded.payload? == '[null,2]'
}
