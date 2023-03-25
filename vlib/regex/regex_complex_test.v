import regex

fn test_complex_matching() {
	tmp_text := r"[export:'something']
const (
some_const   = [1114111, 127, 2047, 65535, 1114111]!

)

[export:'something_empty']
const (
empty_const   = ()
)
"
	query := r'\[export:\S+\]\sconst\s\(\s+\S+\s{3}=\s\('
	mut re := regex.regex_opt(query) or { panic(err) }
	mut a := re.find_all(tmp_text)
	assert a == [86, 138]
}

fn test_complex_matching2() {
	content := '<?xml version="1.0" encoding="utf-8"?>
<resources>
    <string name="v_app_name">V0</string>
    <string name="v_lib_name">v1</string>
    <string name="v_lib_name2">v2</string>
</resources>
'

mut re := regex.regex_opt(r'.*<resources>.+<string name="v_lib_name">([^<]+)') or {
        panic(err)
    }
    
    start, end := re.match_string(content)
    if start >= 0  && re.groups.len > 0 {
    	assert content#[re.groups[0]..re.groups[1]] == 'v1'
    	return
    }
    assert false
}