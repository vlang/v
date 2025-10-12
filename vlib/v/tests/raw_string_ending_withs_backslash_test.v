fn test_raw_string_ending_withs_backslack() {
	str := r'line1 \
	line2 \
        line3\'

	lines := str.split_into_lines()
	assert lines.len == 3
	assert lines[0] == 'line1 \\'
	assert lines[1] == '\tline2 \\'
	assert lines[2] == '        line3\\'
}
