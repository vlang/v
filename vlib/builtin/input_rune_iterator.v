module builtin

// input_rune returns a single rune from the standart input (an unicode codepoint).
// It expects, that the input is utf8 encoded.
// It will return `none` on EOF.
pub fn input_rune() ?rune {
	x := input_character()
	if x <= 0 {
		return none
	}
	char_len := utf8_char_len(u8(x))
	if char_len == 1 {
		return x
	}
	mut b := u8(x)
	b = b << char_len
	mut res := rune(b)
	mut shift := 6 - char_len
	for i := 1; i < char_len; i++ {
		c := rune(input_character())
		res = rune(res) << shift
		res |= c & 63 // 0x3f
		shift = 6
	}
	return res
}

// InputRuneIterator is an iterator over the input runes.
pub struct InputRuneIterator {}

// next returns the next rune from the input stream.
pub fn (mut self InputRuneIterator) next() ?rune {
	return input_rune()
}

// input_rune_iterator returns an iterator to allow for `for i, r in input_rune_iterator() {`.
// When the input stream is closed, the loop will break.
pub fn input_rune_iterator() InputRuneIterator {
	return InputRuneIterator{}
}
