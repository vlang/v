module json2

fn test_str() {
	mut sc := Scanner{ text: '"test"'.bytes() }
	tok := sc.scan()
	assert tok.kind == .str_
	assert tok.lit.len == 4
	assert tok.lit.bytestr() == 'test'
}

fn test_int() {}
