import gg { MouseButton }

enum Token {
	aa = 2
	bb
	cc
}

fn test_map_with_enum_keys() {
	mut m := map[Token]string{}
	m[.aa] = 'abc'
	m[Token.bb] = 'def'
	assert m[Token.aa] == 'abc'
	assert m[.bb] == 'def'
	s := '$m'
	assert s == "{aa: 'abc', bb: 'def'}"
	println(m)
}

fn test_map_with_imported_enum_keys() {
	mut km := map[gg.KeyCode]string{}
	km[.minus] = '-'
	km[gg.KeyCode.comma] = ','
	assert km[.invalid] == ''
	assert gg.KeyCode.comma in km
	assert km[.minus] == '-'
}

fn test_map_with_selective_imported_enum_keys() {
	mut bm := map[MouseButton]string{}
	bm[.left] = 'lb'
	bm[MouseButton.middle] = 'mb'
	assert bm[.left] == 'lb'
	bm.delete(MouseButton.left)
	assert MouseButton.left !in bm
}
