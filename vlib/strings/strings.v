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

pub type Mark = byte | rune | string

fn find_between_pair_byte(input string, start byte, end byte) string {
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

fn find_between_pair_rune(input string, start rune, end rune) string {
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

fn find_between_pair_string(input string, start string, end string) string {
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

// find_between_pair returns the string found between the pair of marks defined
// by `start` and `end`.
// As opposed to the `find_between`, `all_after*`, `all_before*` methods defined on the
// `string` type, this function can extract content between *nested* marks in `input`.
// If `start` and `end` marks are nested in `input` the characters
// between the *outermost* mark pair is returned. It is expected that `start` and `end`
// marks are *balanced*, meaning that both `start` and `end` are of the same type **and**
// the amount of `start` marks equal the amount of `end` marks in the `input`.
// An empty string is returned otherwise. Using two identical marks as `start` and `end`
// results in undefined output behavior.
// find_between_pair works the fastest with marks of type `byte`
// find_between_pair works the slowest with marks of type `string`
// find_between_pair works at speeds inbetween with marks of type `rune`
// Example: assert strings.find_between_pair('/*V*/ /*NOT V*/','/*','*/') == 'V'
// Example: assert strings.find_between_pair('s {X{Y}} s',`{`,`}`) == 'X{Y}'
pub fn find_between_pair(input string, start Mark, end Mark) string {
	if input == '' {
		return ''
	}
	return match start {
		byte {
			find_between_pair_byte(input, start, end as byte)
		}
		rune {
			find_between_pair_rune(input, start, end as rune)
		}
		string {
			find_between_pair_string(input, start, end as string)
		}
	}
}
