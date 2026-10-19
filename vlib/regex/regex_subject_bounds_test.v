import regex

fn bounds_replacement(_ regex.RE, text string, start int, end int) string {
	assert start >= 0 && end <= text.len
	return '#'
}

fn test_non_word_classes_do_not_match_the_end_of_the_subject() {
	for pattern in [r'\W', r'\D', r'\S', r'[^a-z]', r'.'] {
		mut re := regex.regex_opt(pattern) or { panic(err) }
		start, end := re.match_string('')
		assert start == -1 && end == 0, pattern
		assert re.find_all('') == []int{}, pattern
		assert re.find_all_str('') == []string{}, pattern
	}
	for text in ['', 'z', 'zz'] {
		mut re := regex.regex_opt(r'\W') or { panic(err) }
		start, end := re.find(text)
		assert start == -1 && end == -1
		from_start, from_end := re.find_from(text, text.len)
		assert from_start == -1 && from_end == -1
		assert re.find_all(text) == []int{}
		assert re.find_all_str(text) == []string{}
		assert re.split(text) == [text]
		assert re.replace(text, '#') == text
		assert re.replace_simple(text, '#') == text
		assert re.replace_n(text, '#', 1) == text
		assert re.replace_by_fn(text, bounds_replacement) == text
	}
}

fn test_consuming_tokens_stop_at_the_subject_boundary() {
	for pattern in [r'.+', r'\W+', r'[^a-z]+'] {
		mut re := regex.regex_opt(pattern) or { panic(err) }
		start, end := re.find('!!')
		assert start == 0 && end == 2, pattern
		assert re.find_all('!!') == [0, 2], pattern
		assert re.find_all_str('!!') == ['!!'], pattern
	}
	mut required := regex.regex_opt(r'a\W') or { panic(err) }
	start, _ := required.match_string('a')
	assert start == -1
	mut optional := regex.regex_opt(r'a\W?') or { panic(err) }
	opt_start, opt_end := optional.match_string('a')
	assert opt_start == 0 && opt_end == 1
	mut empty := regex.regex_opt(r'a*') or { panic(err) }
	assert empty.find_all('b') == [0, 0, 1, 1]
}

fn test_match_base_uses_the_explicit_length_without_a_terminator() {
	bytes := [u8(`a`), `!`]
	mut re := regex.regex_opt(r'a\W') or { panic(err) }
	start, _ := re.match_base(bytes.data, 1)
	assert start == -1
	full_start, full_end := re.match_base(bytes.data, bytes.len)
	assert full_start == 0 && full_end == 2
	mut any := regex.regex_opt(r'.') or { panic(err) }
	// A truncated multibyte sequence must not read the following bytes.
	utf8_bytes := [u8(0xe2), 0x82, 0xac]
	utf8_start, utf8_end := any.match_base(utf8_bytes.data, 1)
	assert utf8_start == 0 && utf8_end == 1
}

fn test_nul_bytes_inside_the_subject_are_still_matchable() {
	mut re := regex.regex_opt(r'\W') or { panic(err) }
	text := '\x00'
	start, end := re.match_string(text)
	assert start == 0 && end == 1
	assert re.find_all(text) == [0, 1]
	mut any := regex.regex_opt(r'.') or { panic(err) }
	empty_start, empty_end := any.match_base(unsafe { nil }, 0)
	assert empty_start == -1 && empty_end == 0
}

fn test_boundary_keeps_the_final_real_character_for_a_required_token() {
	for pattern in [r'^a+.$', r'^a+a$', r'^[a]+.$', r'^\w+.$', r'^([^a-z]+.)$'] {
		text := if pattern == r'^([^a-z]+.)$' { '!!!' } else { 'aaa' }
		mut re := regex.regex_opt(pattern)!
		start, end := re.match_string(text)
		assert start == 0 && end == text.len, '${pattern}: ${start}, ${end}'
	}
	mut unicode_re := regex.regex_opt(r'^é+.$')!
	start, end := unicode_re.match_string('ééé')
	assert start == 0 && end == 'ééé'.len
}

fn test_exit_on_first_match_respects_the_exact_buffer_length() {
	bytes := [u8(`a`)]
	mut re := regex.regex_opt('a')!
	re.flag |= regex.f_efm
	start, end := re.match_base(bytes.data, bytes.len)
	assert start == 1 && end == 1
}
