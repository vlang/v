import regex

const replace_re = regex.regex_opt(r'(\d+)')!

fn test_replace_on_const_regex() {
	assert replace_re.replace('abc 123 def', r'[\0]') == 'abc [123] def'
	assert replace_re.groups.len == 0
}
