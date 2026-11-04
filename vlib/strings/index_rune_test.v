module strings

fn test_index_rune() {
	assert index_rune('hello', `e`) == 1
	assert index_rune('hello', `z`) == -1
}
