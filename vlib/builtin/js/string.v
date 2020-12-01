module builtin

[js_getter]
pub fn (s V_string) length() JS.Number {
	return s.val.length
}