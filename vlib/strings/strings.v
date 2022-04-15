module strings

// import rand
// random returns a random string with `n` characters
/*
pub fn random(n int) string {
	buf := vmalloc(n)
	for i in 0..n {
		buf[i] = rand.next()
	}
	return tos(buf)
}
*/

// find_between_pair_byte returns the string found between the pair of marks defined
// by `start` and `end`.
// As opposed to the `find_between`, `all_after*`, `all_before*` methods defined on the
// `string` type, this function can extract content between *nested* marks in `input`.
// If `start` and `end` marks are nested in `input`, the characters
// between the *outermost* mark pair is returned. It is expected that `start` and `end`
// marks are *balanced*, meaning that the amount of `start` marks equal the
// amount of `end` marks in the `input`. An empty string is returned otherwise.
// Using two identical marks as `start` and `end` results in undefined output behavior.
// find_between_pair_byte is the fastest in the find_between_pair_* family of functions.
// Example: assert strings.find_between_pair_u8('(V) (NOT V)',`(`,`)`) == 'V'
// Example: assert strings.find_between_pair_u8('s {X{Y}} s',`{`,`}`) == 'X{Y}'
pub fn find_between_pair_u8(input string, start u8, end u8) string {
	mut marks := 0
	mut start_index := -1
	for i, b in input {
		if b == start {
			if start_index == -1 {
				start_index = i + 1
			}
			marks++
			continue
		}
		if start_index > 0 {
			if b == end {
				marks--
				if marks == 0 {
					return input[start_index..i]
				}
			}
		}
	}
	return ''
}

// find_between_pair_rune returns the string found between the pair of marks defined
// by `start` and `end`.
// As opposed to the `find_between`, `all_after*`, `all_before*` methods defined on the
// `string` type, this function can extract content between *nested* marks in `input`.
// If `start` and `end` marks are nested in `input`, the characters
// between the *outermost* mark pair is returned. It is expected that `start` and `end`
// marks are *balanced*, meaning that the amount of `start` marks equal the
// amount of `end` marks in the `input`. An empty string is returned otherwise.
// Using two identical marks as `start` and `end` results in undefined output behavior.
// find_between_pair_rune is inbetween the fastest and slowest in the find_between_pair_* family of functions.
// Example: assert strings.find_between_pair_rune('(V) (NOT V)',`(`,`)`) == 'V'
// Example: assert strings.find_between_pair_rune('s {X{Y}} s',`{`,`}`) == 'X{Y}'
pub fn find_between_pair_rune(input string, start rune, end rune) string {
	mut marks := 0
	mut start_index := -1
	runes := input.runes()
	for i, r in runes {
		if r == start {
			if start_index == -1 {
				start_index = i + 1
			}
			marks++
			continue
		}
		if start_index > 0 {
			if r == end {
				marks--
				if marks == 0 {
					return runes[start_index..i].string()
				}
			}
		}
	}
	return ''
}

// find_between_pair_string returns the string found between the pair of marks defined
// by `start` and `end`.
// As opposed to the `find_between`, `all_after*`, `all_before*` methods defined on the
// `string` type, this function can extract content between *nested* marks in `input`.
// If `start` and `end` marks are nested in `input`, the characters
// between the *outermost* mark pair is returned. It is expected that `start` and `end`
// marks are *balanced*, meaning that the amount of `start` marks equal the
// amount of `end` marks in the `input`. An empty string is returned otherwise.
// Using two identical marks as `start` and `end` results in undefined output behavior.
// find_between_pair_string is the slowest in the find_between_pair_* function family.
// Example: assert strings.find_between_pair_string('/*V*/ /*NOT V*/','/*','*/') == 'V'
// Example: assert strings.find_between_pair_string('s {{X{{Y}}}} s','{{','}}') == 'X{{Y}}'
pub fn find_between_pair_string(input string, start string, end string) string {
	mut start_index := -1
	mut marks := 0
	start_runes := start.runes()
	end_runes := end.runes()
	runes := input.runes()
	mut i := 0
	for ; i < runes.len; i++ {
		start_slice := runes#[i..i + start_runes.len]
		if start_slice == start_runes {
			i = i + start_runes.len - 1
			if start_index < 0 {
				start_index = i + 1
			}
			marks++
			continue
		}
		if start_index > 0 {
			end_slice := runes#[i..i + end_runes.len]
			if end_slice == end_runes {
				marks--
				if marks == 0 {
					return runes[start_index..i].string()
				}
				i = i + end_runes.len - 1
				continue
			}
		}
	}
	return ''
}
