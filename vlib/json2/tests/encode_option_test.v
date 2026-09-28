import json2

struct JoseHeader {
pub mut:
	cty ?string
	alg string
	typ string = 'JWT'
}

type OptionElem = int | ?int

type OptionName = ?string | int

fn test_main() {
	res := json2.encode(JoseHeader{ alg: 'HS256' })
	assert res == '{"alg":"HS256","typ":"JWT"}'
}

fn test_option_sumtype_variants() {
	empty := ?int(none)
	// Like the removed module, a `none` option variant is written as `{}`.
	assert json2.encode([OptionElem(1), OptionElem(empty), 3]) == '[1,{},3]'
	assert json2.encode([OptionElem(1), OptionElem(?int(5)), 3]) == '[1,5,3]'
	no_name := ?string(none)
	assert json2.encode(OptionName(no_name)) == '{}'
	assert json2.encode(OptionName(?string('x"y'))) == '"x\\"y"'
}

fn test_option_values_outside_struct_fields() {
	none_int := ?int(none)
	assert json2.encode(none_int) == 'null'
	assert json2.encode(?int(7)) == '7'
	assert json2.encode([?int(1), none, 3]) == '[1,null,3]'
	assert json2.encode({
		'a': ?int(1)
		'b': none
	}) == '{"a":1,"b":null}'
}
