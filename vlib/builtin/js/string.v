module builtin

pub struct string {
	str JS.String
}

pub fn (s string) slice(a int, b int) string {
	return string(s.str.slice(a, b))
}

pub fn (s string) after(dot string) string {
	return string(s.str.slice(s.str.lastIndexOf(dot) + 1, int(s.str.length)))
}


pub fn (s string) after_char(dot byte) string {
	// TODO: Implement after byte
	return s
}

pub fn (s string) all_after(dot string) string {
	return string(s.str.slice(s.str.indexOf(dot) + 1, int(s.str.length)))
}

pub fn (s string) all_after_last(dot string) string {
	return string(s.str.slice(s.str.indexOf(dot) + 1, int(s.str.length)))
}

pub fn (s string) all_before(dot string) string {
	return string(s.str.slice(0, s.str.indexOf(dot)))
}
