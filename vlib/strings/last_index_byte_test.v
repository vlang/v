module strings

fn test_last_index_byte() {
	assert last_index_byte('hello', `l`) == 3
	assert last_index_byte('hello', `z`) == -1
}
