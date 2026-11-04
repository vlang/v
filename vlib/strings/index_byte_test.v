module strings

fn test_index_byte() {
	assert index_byte('hello', `e`) == 1
	assert index_byte('hello', `z`) == -1
}
