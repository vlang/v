// A string, number or boolean decodes into the plain variant rather than an alias
// of the same type, like in the removed `json` module.
import json2

type MyString = string
type MyInt = int
type MyBool = bool
type StrSum = MyString | int | string
type NumSum = MyInt | int
type BoolSum = MyBool | bool
type OnlyAlias = MyString | int

fn test_plain_variant_wins_over_alias() {
	s := json2.decode[StrSum]('"foo"')!
	assert s.type_name() == 'string'
	n := json2.decode[NumSum]('3')!
	assert n.type_name() == 'int'
	b := json2.decode[BoolSum]('true')!
	assert b.type_name() == 'bool'
	a := json2.decode[OnlyAlias]('"x"')!
	assert a.type_name() == 'MyString'
	num := json2.decode[OnlyAlias]('5')!
	assert num.type_name() == 'int'
}

struct Holder {
	field StrSum
}

fn test_struct_field_plain_variant_wins_over_alias() {
	h := json2.decode[Holder]('{"field":"foo"}')!
	assert h.field.type_name() == 'string'
	assert h.field == StrSum('foo')
}
