module progress

fn test_frame_sets_are_sane() {
	assert spinner_table.len == 76
	assert spinner_set_count() == spinner_table.len
	for i, set in spinner_table {
		assert set.len > 0, 'set ${i} is empty'
		for f in set {
			assert f.len > 0, 'set ${i} has an empty frame'
		}
	}
	assert spinner_dots() == spinner_table[14]
	assert spinner_line() == ['|', '/', '-', '\\']
	// every named preset is a real set
	for set in [spinner_dots(), spinner_braille(), spinner_line(), spinner_arrows(), spinner_circle(),
		spinner_quarters(), spinner_grow(), spinner_bounce(), spinner_pulse(), spinner_ellipsis(),
		spinner_earth(), spinner_moon()] {
		assert set.len > 1
	}
}

fn test_spinner_set_returns_independent_copies() {
	mut a := spinner_set(9)
	a[0] = 'CHANGED'
	assert spinner_set(9)[0] == '|' // the table is untouched
	assert spinner_line()[0] == '|'
	assert spinner_set(0) == spinner_arrows()
}
