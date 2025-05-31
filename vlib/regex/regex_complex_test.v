import regex

fn test_complex_matching001() {
	tmp_text := r"@[export:'something']
const (
some_const   = [1114111, 127, 2047, 65535, 1114111]!

)

@[export:'something_empty']
const (
empty_const   = ()
)
"
	query := r'\[export:\S+\]\sconst\s\(\s+\S+\s{3}=\s\('
	mut re := regex.regex_opt(query) or { panic(err) }
	mut a := re.find_all(tmp_text)
	assert a == [88, 140]
}

fn test_complex_matching002() {
	text := '<?xml version="1.0" encoding="utf-8"?>
<resources>
    <string name="v_app_name">V0</string>
    <!-- comment -->
    <string name="v_lib_name">v1</string>
    <string name="v_lib_name2">v2</string>
</resources>
'

	mut re := regex.regex_opt(r'.*<resources>.+<string name="v_lib_name">([^<]+)') or { panic(err) }

	start, end := re.match_string(text)
	if start >= 0 && re.groups.len > 0 {
		// check that we have obtained our 'v1' value
		assert text#[re.groups[0]..re.groups[1]] == 'v1'
		return
	}
	assert false
}

fn test_complex_matching003() {
	text := 'abcdefgt1234abcd<tag name="mio_tag">value</tag>'

	mut re := regex.regex_opt(r'\w*<tag\s*name\s*="mio_tag">([^<]+)') or { panic(err) }

	start, end := re.match_string(text)
	if start >= 0 && re.groups.len > 0 {
		println('found ${text#[start..end]}')
		println('group: ${text#[re.groups[0]..re.groups[1]]}')
		return
	}
	assert false
}
