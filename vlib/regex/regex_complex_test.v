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
