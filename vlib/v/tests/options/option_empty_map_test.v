fn opt() ?map[string]string {
	return {}
}

fn test_option_empty_map() {
	x := opt() or {
		assert false
		return
	}

	dump(x)
	assert '${x}' == '{}'
}
