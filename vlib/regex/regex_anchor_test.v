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
