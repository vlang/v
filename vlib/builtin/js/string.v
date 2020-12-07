module builtin

pub struct string {
	str JS.String
}

pub fn (s string) slice(a int, b int) string {
	return string(s.str.slice(a, b))
}
