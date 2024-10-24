module main

import encoding.utf8 as u8

fn check_char(s &string) int {
	for i, ch in s.runes() {
		is_p := u8.is_punct(ch.str(), 0)
		okch := ch.str()
		if !is_p {
			println('${if is_p { ' is punct' } else { 'not punct' }}, ${u8.get_rune(s,
				i)}, ${i} ${okch}')
			assert false
		}
	}
	return 0
}

fn test_main() {
	a := "!.,:;',/*"
	assert check_char(a) == 0
}
