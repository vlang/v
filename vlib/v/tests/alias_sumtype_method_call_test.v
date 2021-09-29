import x.json2

type MyType = json2.Any

struct Data {
	prop MyType
}

fn test_alias_sumtype_method_call() {
	a := '{"a":"a","b":1}'
	json := json2.raw_decode(a) or { panic(err) }
	data := Data{json}
	json_str := data.prop.str()
	println(json_str)
	assert json_str == '{"a":"a","b":1}'
}
