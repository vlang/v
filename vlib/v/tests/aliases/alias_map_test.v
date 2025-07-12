module main

type Dict = map[string]string

fn test_main() {
	mut x := Dict{}
	x['foo'] = 'bar'
	assert '${x}' == "Dict({'foo': 'bar'})"
}
