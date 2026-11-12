fn test_array_map_if_byte_keeps_byte_element_type() {
	name := 'r-draw_2.c'
	mapped := name.bytes().map(if it.is_alnum() || it == `_` { it } else { `_` })
	assert typeof(mapped).name == '[]u8'
	assert mapped.bytestr() == 'r_draw_2_c'
	assert name.bytes().map(if it.is_alnum() || it == `_` { it } else { `_` }).bytestr() == 'r_draw_2_c'
}

fn test_array_map_if_byte_with_else_if() {
	bytes := 'a-\t'.bytes()
	mapped := bytes.map(if it.is_alnum() {
		it
	} else if it == `-` {
		`_`
	} else {
		`\n`
	})
	assert typeof(mapped).name == '[]u8'
	assert mapped.bytestr() == 'a_\n'
	assert ''.bytes().map(if it.is_alnum() { it } else { `_` }).bytestr() == ''
}

fn test_array_map_if_keeps_explicit_rune_results() {
	bytes := 'a-'.bytes()
	mapped := bytes.map(if it.is_alnum() { rune(it) } else { `λ` })
	assert typeof(mapped).name == '[]rune'
	assert mapped == [`a`, `λ`]
	wide := bytes.map(if it.is_alnum() { u32(it) } else { `_` })
	assert typeof(wide).name == '[]u32'
	assert wide == [u32(`a`), u32(`_`)]
}
