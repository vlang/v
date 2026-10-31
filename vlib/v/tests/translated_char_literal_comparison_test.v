@[translated]
module main

const eof = -1

// C translated by c2v compares an `int` with character constants such as
// `','`, which are `int`s too: EOF (-1) is not greater than `','`. SQLite's CSV
// reader stops a field at `while (c > ',' || (c != EOF && ...))`.
fn field_length(s &u8, n int) int {
	mut i := 0
	mut c := if n > 0 { int(s[0]) } else { eof }
	for c > `,` || (c != eof && c != `,` && c != `\n`) {
		i++
		c = if i < n { int(unsafe { s[i] }) } else { eof }
	}
	return i
}

fn test_int_compared_with_a_char_literal_is_signed() {
	c := i32(-1)
	assert !(c > `,`)
	assert c < `,`
	assert field_length(c'abcd', 4) == 4
	assert field_length(c'ab,cd', 5) == 2
	u := u32(0xffff_ffff)
	assert u > `,`
}
