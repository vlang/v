// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module builtin

import strings

// This was never working correctly, the issue is now
// fixed however the type checks in checker need to be
// updated. if you uncomment it you will see the issue
// type rune = int

// for unicode type fast lookup
const (
	p_c      = 1 // a control character.
	p_p      = 2 // a punctuation character.
	p_n      = 4 // a numeral.
	p_s      = 8 // a symbolic character.
	p_z      = 16 // a spacing character.
	p_lu     = 32 // an up_prer-case letter.
	p_ll     = 64 // a lower-case letter.
	p_pr     = 128 // a printable character according to Go's definition.
	p_g      = p_pr | p_z // a graphical character according to the Unicode definition.
	p_lo     = p_lu | p_ll // a letter that is neither up_prer nor lower case.
	p_l_mask = p_lo
)

const props = [
	p_c, // '\x00'
	p_c, // '\x01'
	p_c, // '\x02'
	p_c, // '\x03'
	p_c, // '\x04'
	p_c, // '\x05'
	p_c, // '\x06'
	p_c, // '\a'
	p_c, // '\b'
	p_c, // '\t'
	p_c, // '\n'
	p_c, // '\v'
	p_c, // '\f'
	p_c, // '\r'
	p_c, // '\x0e'
	p_c, // '\x0f'
	p_c, // '\x10'
	p_c, // '\x11'
	p_c, // '\x12'
	p_c, // '\x13'
	p_c, // '\x14'
	p_c, // '\x15'
	p_c, // '\x16'
	p_c, // '\x17'
	p_c, // '\x18'
	p_c, // '\x19'
	p_c, // '\x1a'
	p_c, // '\x1b'
	p_c, // '\x1c'
	p_c, // '\x1d'
	p_c, // '\x1e'
	p_c, // '\x1f'
	p_z | p_pr, // ' '
	p_p | p_pr, // '!'
	p_p | p_pr, // '"'
	p_p | p_pr, // '#'
	p_s | p_pr, // '$'
	p_p | p_pr, // '%'
	p_p | p_pr, // '&'
	p_p | p_pr, // '\''
	p_p | p_pr, // '('
	p_p | p_pr, // ')'
	p_p | p_pr, // '*'
	p_s | p_pr, // '+'
	p_p | p_pr, // ','
	p_p | p_pr, // '-'
	p_p | p_pr, // '.'
	p_p | p_pr, // '/'
	p_n | p_pr, // '0'
	p_n | p_pr, // '1'
	p_n | p_pr, // '2'
	p_n | p_pr, // '3'
	p_n | p_pr, // '4'
	p_n | p_pr, // '5'
	p_n | p_pr, // '6'
	p_n | p_pr, // '7'
	p_n | p_pr, // '8'
	p_n | p_pr, // '9'
	p_p | p_pr, // ':'
	p_p | p_pr, // ';'
	p_s | p_pr, // '<'
	p_s | p_pr, // '='
	p_s | p_pr, // '>'
	p_p | p_pr, // '?'
	p_p | p_pr, // '@'
	p_lu | p_pr, // 'A'
	p_lu | p_pr, // 'B'
	p_lu | p_pr, // 'C'
	p_lu | p_pr, // 'D'
	p_lu | p_pr, // 'E'
	p_lu | p_pr, // 'F'
	p_lu | p_pr, // 'G'
	p_lu | p_pr, // 'H'
	p_lu | p_pr, // 'I'
	p_lu | p_pr, // 'J'
	p_lu | p_pr, // 'K'
	p_lu | p_pr, // 'L'
	p_lu | p_pr, // 'M'
	p_lu | p_pr, // 'N'
	p_lu | p_pr, // 'O'
	p_lu | p_pr, // 'P'
	p_lu | p_pr, // 'Q'
	p_lu | p_pr, // 'R'
	p_lu | p_pr, // 'S'
	p_lu | p_pr, // 'T'
	p_lu | p_pr, // 'U'
	p_lu | p_pr, // 'V'
	p_lu | p_pr, // 'W'
	p_lu | p_pr, // 'X'
	p_lu | p_pr, // 'Y'
	p_lu | p_pr, // 'Z'
	p_p | p_pr, // '['
	p_p | p_pr, // '\\'
	p_p | p_pr, // ']'
	p_s | p_pr, // '^'
	p_p | p_pr, // '_'
	p_s | p_pr, // '`'
	p_ll | p_pr, // 'a'
	p_ll | p_pr, // 'b'
	p_ll | p_pr, // 'c'
	p_ll | p_pr, // 'd'
	p_ll | p_pr, // 'e'
	p_ll | p_pr, // 'f'
	p_ll | p_pr, // 'g'
	p_ll | p_pr, // 'h'
	p_ll | p_pr, // 'i'
	p_ll | p_pr, // 'j'
	p_ll | p_pr, // 'k'
	p_ll | p_pr, // 'l'
	p_ll | p_pr, // 'm'
	p_ll | p_pr, // 'n'
	p_ll | p_pr, // 'o'
	p_ll | p_pr, // 'p'
	p_ll | p_pr, // 'q'
	p_ll | p_pr, // 'r'
	p_ll | p_pr, // 's'
	p_ll | p_pr, // 't'
	p_ll | p_pr, // 'u'
	p_ll | p_pr, // 'v'
	p_ll | p_pr, // 'w'
	p_ll | p_pr, // 'x'
	p_ll | p_pr, // 'y'
	p_ll | p_pr, // 'z'
	p_p | p_pr, // '{'
	p_s | p_pr, // '|'
	p_p | p_pr, // '}'
	p_s | p_pr, // '~'
	p_c, // '\u007f'
	p_c, // '\u0080'
	p_c, // '\u0081'
	p_c, // '\u0082'
	p_c, // '\u0083'
	p_c, // '\u0084'
	p_c, // '\u0085'
	p_c, // '\u0086'
	p_c, // '\u0087'
	p_c, // '\u0088'
	p_c, // '\u0089'
	p_c, // '\u008a'
	p_c, // '\u008b'
	p_c, // '\u008c'
	p_c, // '\u008d'
	p_c, // '\u008e'
	p_c, // '\u008f'
	p_c, // '\u0090'
	p_c, // '\u0091'
	p_c, // '\u0092'
	p_c, // '\u0093'
	p_c, // '\u0094'
	p_c, // '\u0095'
	p_c, // '\u0096'
	p_c, // '\u0097'
	p_c, // '\u0098'
	p_c, // '\u0099'
	p_c, // '\u009a'
	p_c, // '\u009b'
	p_c, // '\u009c'
	p_c, // '\u009d'
	p_c, // '\u009e'
	p_c, // '\u009f'
	p_z, // '\u00a0'
	p_p | p_pr, // '¡'
	p_s | p_pr, // '¢'
	p_s | p_pr, // '£'
	p_s | p_pr, // '¤'
	p_s | p_pr, // '¥'
	p_s | p_pr, // '¦'
	p_p | p_pr, // '§'
	p_s | p_pr, // '¨'
	p_s | p_pr, // '©'
	p_lo | p_pr, // 'ª'
	p_p | p_pr, // '«'
	p_s | p_pr, // '¬'
	0, // '\u00ad'
	p_s | p_pr, // '®'
	p_s | p_pr, // '¯'
	p_s | p_pr, // '°'
	p_s | p_pr, // '±'
	p_n | p_pr, // '²'
	p_n | p_pr, // '³'
	p_s | p_pr, // '´'
	p_ll | p_pr, // 'µ'
	p_p | p_pr, // '¶'
	p_p | p_pr, // '·'
	p_s | p_pr, // '¸'
	p_n | p_pr, // '¹'
	p_lo | p_pr, // 'º'
	p_p | p_pr, // '»'
	p_n | p_pr, // '¼'
	p_n | p_pr, // '½'
	p_n | p_pr, // '¾'
	p_p | p_pr, // '¿'
	p_lu | p_pr, // 'À'
	p_lu | p_pr, // 'Á'
	p_lu | p_pr, // 'Â'
	p_lu | p_pr, // 'Ã'
	p_lu | p_pr, // 'Ä'
	p_lu | p_pr, // 'Å'
	p_lu | p_pr, // 'Æ'
	p_lu | p_pr, // 'Ç'
	p_lu | p_pr, // 'È'
	p_lu | p_pr, // 'É'
	p_lu | p_pr, // 'Ê'
	p_lu | p_pr, // 'Ë'
	p_lu | p_pr, // 'Ì'
	p_lu | p_pr, // 'Í'
	p_lu | p_pr, // 'Î'
	p_lu | p_pr, // 'Ï'
	p_lu | p_pr, // 'Ð'
	p_lu | p_pr, // 'Ñ'
	p_lu | p_pr, // 'Ò'
	p_lu | p_pr, // 'Ó'
	p_lu | p_pr, // 'Ô'
	p_lu | p_pr, // 'Õ'
	p_lu | p_pr, // 'Ö'
	p_s | p_pr, // '×'
	p_lu | p_pr, // 'Ø'
	p_lu | p_pr, // 'Ù'
	p_lu | p_pr, // 'Ú'
	p_lu | p_pr, // 'Û'
	p_lu | p_pr, // 'Ü'
	p_lu | p_pr, // 'Ý'
	p_lu | p_pr, // 'Þ'
	p_ll | p_pr, // 'ß'
	p_ll | p_pr, // 'à'
	p_ll | p_pr, // 'á'
	p_ll | p_pr, // 'â'
	p_ll | p_pr, // 'ã'
	p_ll | p_pr, // 'ä'
	p_ll | p_pr, // 'å'
	p_ll | p_pr, // 'æ'
	p_ll | p_pr, // 'ç'
	p_ll | p_pr, // 'è'
	p_ll | p_pr, // 'é'
	p_ll | p_pr, // 'ê'
	p_ll | p_pr, // 'ë'
	p_ll | p_pr, // 'ì'
	p_ll | p_pr, // 'í'
	p_ll | p_pr, // 'î'
	p_ll | p_pr, // 'ï'
	p_ll | p_pr, // 'ð'
	p_ll | p_pr, // 'ñ'
	p_ll | p_pr, // 'ò'
	p_ll | p_pr, // 'ó'
	p_ll | p_pr, // 'ô'
	p_ll | p_pr, // 'õ'
	p_ll | p_pr, // 'ö'
	p_s | p_pr, // '÷'
	p_ll | p_pr, // 'ø'
	p_ll | p_pr, // 'ù'
	p_ll | p_pr, // 'ú'
	p_ll | p_pr, // 'û'
	p_ll | p_pr, // 'ü'
	p_ll | p_pr, // 'ý'
	p_ll | p_pr, // 'þ'
	p_ll | p_pr, // 'ÿ'
]!

pub fn (c rune) str() string {
	return utf32_to_str(u32(c))
	/*
	unsafe {
		fst_byte := int(c)>>8 * 3 & 0xff
		len := utf8_char_len(byte(fst_byte))
		println('len=$len')
		mut str := string{
			len: len
			str: malloc(len + 1)
		}
		for i in 0..len {
			str.str[i] = byte(int(c)>>8 * (3 - i) & 0xff)
		}
		str.str[len] = `\0`
		println(str)
		return str
	}
	*/
}

// string converts a rune array to a string
[manualfree]
pub fn (ra []rune) string() string {
	mut sb := strings.new_builder(ra.len)
	sb.write_runes(ra)
	res := sb.str()
	unsafe { sb.free() }
	return res
}

// is_letter returns `true` if the rune is in range a-z or A-Z and `false` otherwise.
pub fn (ra rune) is_letter() bool {
	if (ra >= `a` && ra <= `z`) || (ra >= `A` && ra <= `Z`) {
		return true
	} else if ra <= max_latin_1r {
		return props[byte(ra)] & p_l_mask != 0
	}
	return ra.is_excluding_latin(letter_table)
}

// Define this on byte as well, so that we can do `s[0].is_capital()`
pub fn (c byte) is_capital() bool {
	return c >= `A` && c <= `Z`
}

pub fn (b []byte) clone() []byte {
	mut res := []byte{len: b.len}
	// mut res := make([]byte, {repeat:b.len})
	for i in 0 .. b.len {
		res[i] = b[i]
	}
	return res
}

// TODO: remove this once runes are implemented
pub fn (b []byte) bytestr() string {
	unsafe {
		buf := malloc_noscan(b.len + 1)
		vmemcpy(buf, b.data, b.len)
		buf[b.len] = 0
		return tos(buf, b.len)
	}
}
