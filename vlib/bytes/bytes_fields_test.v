module bytes

fn test_fields() {
	assert fields('  a b  c '.bytes()) == ['a'.bytes(), 'b'.bytes(), 'c'.bytes()]
	assert fields(''.bytes()) == [][]u8{}
	assert fields('   '.bytes()) == [][]u8{}
}
