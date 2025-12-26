module builtin

pub fn (ra []rune) string() string {
	mut sb := new_string_builder(cap: ra.len)
	sb.write_runes(ra)
	res := sb.str()
	return res
}

pub fn (c rune) repeat(count int) string {
	if count <= 0 {
		return ''
	} else if count == 1 {
		return c.str()
	}
	res := ''
	#res.str = String.fromCharCode(Number(c.val))

	return res.repeat(count)
}

pub fn (c rune) str() string {
	res := ''
	#res.str = String.fromCharCode(Number(c.val))

	return res
}
