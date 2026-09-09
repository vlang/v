import regex

fn test_anchor_start() {
	mut re := regex.regex_opt(r'^\w+') or { panic(err) }
	start, end := re.find('id.')
	assert start == 0
	assert end == 2
}

fn test_anchor_end() {
	mut re := regex.regex_opt(r'\w+$') or { panic(err) }
	start, end := re.find('(id')
	assert start == 1
	assert end == 3
}

fn test_anchor_both() {
	mut re := regex.regex_opt(r'^\w+$') or { panic(err) }
	start, end := re.find('(id)')
	assert start == -1
	assert end == -1
}

fn test_anchor_both_find_multiline() {
	text := 'TITLE\n\nThis is a test.'
	mut re := regex.regex_opt(r'^\w+$') or { panic(err) }
	start, end := re.find(text)
	assert start == 0
	assert end == 5
}

fn test_anchor_both_find_all_multiline() {
	text := 'TITLE\n\nThis is a test.'
	mut re := regex.regex_opt(r'^\w+$') or { panic(err) }
	res := re.find_all(text)
	assert res == [0, 5]
}

fn test_anchor_both_find_all_str_multiline() {
	text := 'TITLE\n\nThis is a test.'
	mut re := regex.regex_opt(r'^\w+$') or { panic(err) }
	res := re.find_all_str(text)
	assert res == ['TITLE']
}
