type Block = [8]byte

fn test_alias_fixed_array() {
	a := [8]byte{init: 22}
	ret := get(Block(a))
	println(ret)
	assert ret == 'Block([22, 22, 22, 22, 22, 22, 22, 22])'
}

fn get(b Block) string {
	return '$b'
}
