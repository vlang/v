fn some_fn(text string) string {
	mut some_map := map[string]string{}

	key := ''
	found := some_map[key] or {
		if true {
			if true {
			} else {
			}
		} else {
		}

		'no'
	}
	return found
}

fn test_or_block_endwiths_expr() {
	assert some_fn('abc abc') == 'no'
}
