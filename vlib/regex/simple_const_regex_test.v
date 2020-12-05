import regex

const (
	a_or_b = regex.regex_opt('a|b') ?
)

fn f(s string) bool {
	mut re := a_or_b
	start, _ := re.match_string(s)
	return start != -1
}

fn test_const_regex_works() {
	assert f('a') == true
	assert f('b') == true
	assert f('c') == false
}
