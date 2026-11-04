module strings

fn test_last_index_rune() {
	assert last_index_rune('hello', `l`) == 3
	assert last_index_rune('hello', `z`) == -1
}
