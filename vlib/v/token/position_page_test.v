module token

fn assert_file_lines_match_scan(f &File, source string) {
	mut expected := 1
	assert f.find_line(-1) == 0
	for pos in 0 .. source.len + 513 {
		if pos > 0 && pos <= source.len && source[pos - 1] == `\n` {
			expected++
		}
		assert f.find_line(pos) == expected, 'offset ${pos}'
	}
}

fn test_line_pages_preserve_positions_at_page_and_line_boundaries() {
	for width in [1, 2, 7, 31, 255, 256, 257, 600] {
		source := ('x'.repeat(width - 1) + '\n').repeat(40) + 'tail'
		mut f := File{
			name: 'lines.v'
			size: source.len
		}
		f.index_line_starts(source)
		assert f.line_pages.len > 0
		assert_file_lines_match_scan(f, source)
		cloned := f.clone_index()
		assert_file_lines_match_scan(cloned, source)
		f.index_line_starts('short\nfile')
		assert f.line_pages.len == 0
		assert_file_lines_match_scan(f, 'short\nfile')
		assert_file_lines_match_scan(cloned, source)
	}
}

fn test_line_pages_are_invalidated_when_lines_are_appended_or_reset() {
	source := 'x\n'.repeat(40)
	mut f := File{
		name: 'growing.v'
		size: source.len + 20
	}
	f.index_line_starts(source)
	f.add_line(source.len + 10)
	assert f.line_pages.len == 0
	assert f.find_line(source.len + 9) == 41
	assert f.find_line(source.len + 10) == 42
	f.index_line_starts(source)
	f.index_lines_without_digest(source)
	assert f.line_pages.len == 0
	assert f.find_line(0) == 1
	assert f.find_line(source.len) == 1
}
