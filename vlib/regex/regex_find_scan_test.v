import regex

// One row of the anchor tables: a pattern, the text it runs against, and the
// (start, end) pair the matcher must report.
struct ScanCase {
	pattern string
	text    string
	start   int
	end     int
}

// `match_string` is anchored at offset 0: the pattern has to match from the
// first byte, and `^`/`$` are the only way to widen that.
const anchored_match_cases = [
	ScanCase{r'^abc$', 'abc', 0, 3},
	ScanCase{r'^abc$', 'abcd', -1, 0},
	ScanCase{r'abc', 'abc', 0, 3},
	ScanCase{r'abc', 'xabc', -1, 0},
	ScanCase{r'^abc', 'abc', 0, 3},
	ScanCase{r'^abc', 'xabc', -1, 0},
	ScanCase{r'abc$', 'abc', 0, 3},
	ScanCase{r'abc$', 'abcx', -1, 0},
	// `$` is satisfied by the position right before a trailing newline
	ScanCase{r'^abc$', 'abc\n', 0, 3},
	ScanCase{r'^', 'abc', 0, 0},
	ScanCase{r'$', 'abc', -1, 0},
	// `^` and `$` are only anchors at the two ends of the pattern
	ScanCase{r'a$b', 'a$b', 0, 3},
	ScanCase{r'a^b', 'a$b', -1, 1},
	ScanCase{r'a', 'aaa', 0, 1},
	ScanCase{r'\w+$', 'ab\ncd', 0, 2},
]

// `find` scans forward from offset 0 until the pattern matches, so the same
// pattern that refuses `match_string` can succeed here.
const find_match_cases = [
	ScanCase{r'abc', 'xabcx', 1, 4},
	ScanCase{r'abc$', 'xabc', 1, 4},
	ScanCase{r'abc$', 'abcx', -1, -1},
	ScanCase{r'^abc', 'xabc', -1, -1},
	ScanCase{r'^abc$', 'abcd', -1, -1},
	ScanCase{r'a', 'aaa', 0, 1},
	ScanCase{r'\w+$', 'one two\n', 4, 7},
]

const find_all_cases = [
	ScanCase{r'\d+', 'abcd 1234 efgh', 0, 0},
	ScanCase{r'a*', 'aaa', 0, 0},
	ScanCase{r'a*', 'b', 0, 0},
	ScanCase{r'a*', '', 0, 0},
	ScanCase{r',', 'a,b,,c', 0, 0},
]

const find_all_expected = [
	[int(5), 9],
	[int(0), 3, 3, 3],
	[int(0), 0, 1, 1],
	[int(0), 0],
	[int(1), 2, 3, 4, 4, 5],
]

const find_all_str_expected = [
	['1234'],
	['aaa', ''],
	['', ''],
	[''],
	[',', ',', ','],
]

// One row of the `find_from` table: where to start scanning, and the
// (start, end) pair that must come back.
struct FromCase {
	from       int
	want_start int
	want_end   int
}

const find_from_cases = [
	FromCase{-1, -1, -1},
	FromCase{0, 0, 3},
	FromCase{2, 2, 3},
	FromCase{3, -1, -1},
	FromCase{9, -1, -1},
]

fn test_match_string_is_anchored_at_the_start_of_the_text() {
	for c in anchored_match_cases {
		mut re := regex.regex_opt(c.pattern) or { panic(err) }
		start, end := re.match_string(c.text)
		msg := 'pattern "${c.pattern}" on "${c.text}"'
		assert start == c.start, '${msg}: start ${start}, want ${c.start}'
		assert end == c.end, '${msg}: end ${end}, want ${c.end}'
	}
}

fn test_find_scans_forward_from_offset_zero() {
	for c in find_match_cases {
		mut re := regex.regex_opt(c.pattern) or { panic(err) }
		start, end := re.find(c.text)
		msg := 'pattern "${c.pattern}" on "${c.text}"'
		assert start == c.start, '${msg}: start ${start}, want ${c.start}'
		assert end == c.end, '${msg}: end ${end}, want ${c.end}'
	}
}

fn test_matches_string_agrees_with_match_string() {
	for c in anchored_match_cases {
		mut re := regex.regex_opt(c.pattern) or { panic(err) }
		assert re.matches_string(c.text) == (c.start >= 0), 'pattern "${c.pattern}" on "${c.text}"'
	}
}

fn test_find_all_returns_start_end_pairs() {
	for i, c in find_all_cases {
		mut re := regex.regex_opt(c.pattern) or { panic(err) }
		assert re.find_all(c.text) == find_all_expected[i], 'pattern "${c.pattern}" on "${c.text}"'
	}
}

fn test_find_all_str_returns_the_matched_substrings() {
	for i, c in find_all_cases {
		mut re := regex.regex_opt(c.pattern) or { panic(err) }
		assert re.find_all_str(c.text) == find_all_str_expected[i], 'pattern "${c.pattern}" on "${c.text}"'
	}
}

// `find_all_str` must report exactly the text between each (start, end) pair
// that `find_all` reports, for every shape of pattern.
fn test_find_all_str_agrees_with_find_all_indexes() {
	patterns := [r'\d+', r'\a+', r'a*', r',', r'\s', r'(ab)+', r'x', r'[ab]']
	texts := ['', 'a', 'ab 12 cd', 'a,b,,c', 'abab', '   ', 'xyz']
	for pattern in patterns {
		for text in texts {
			mut re := regex.regex_opt(pattern) or { panic(err) }
			indexes := re.find_all(text)
			strs := re.find_all_str(text)
			mut rebuilt := []string{}
			for i := 0; i < indexes.len; i += 2 {
				rebuilt << text#[indexes[i]..indexes[i + 1]]
			}
			assert rebuilt == strs, 'pattern "${pattern}" on "${text}": find_all ${indexes} vs find_all_str ${strs}'
		}
	}
}

fn test_find_from_start_index() {
	for c in find_from_cases {
		mut re := regex.regex_opt(r'\d+') or { panic(err) }
		start, end := re.find_from('123', c.from)
		msg := '\\d+ on "123" from ${c.from}'
		assert start == c.want_start, '${msg}: start ${start}, want ${c.want_start}'
		assert end == c.want_end, '${msg}: end ${end}, want ${c.want_end}'
	}
}

fn test_find_all_is_bounded_by_the_start_anchor() {
	mut re := regex.regex_opt(r'^\w+$') or { panic(err) }
	assert re.find_all('one\ntwo') == [0, 3]

	mut unanchored := regex.regex_opt(r'\w+') or { panic(err) }
	assert unanchored.find_all('one\ntwo') == [0, 3, 4, 7]
}

// A zero-length match is still a match: `a*` reports one empty span per
// position, which is what makes `find_all` count the input length.
fn test_zero_length_matches_are_reported_for_every_position() {
	mut re := regex.regex_opt(r'a*') or { panic(err) }
	assert re.find_all('') == [0, 0]
	assert re.find_all('b') == [0, 0, 1, 1]
	assert re.find_all_str('') == ['']
	assert re.find_all_str('b') == ['', '']
	empty_start, empty_end := re.find('')
	assert empty_start == 0 && empty_end == 0
	b_start, b_end := re.find('b')
	assert b_start == 0 && b_end == 0
	from_start, from_end := re.find_from('b', 1)
	assert from_start == 1 && from_end == 1
	assert re.matches_string('b')
}

// `\d+` cannot match the empty string, so every finder must come back empty
// rather than reporting a zero-width span at the end.
fn test_no_match_is_reported_without_a_zero_length_span() {
	mut re := regex.regex_opt(r'\d+') or { panic(err) }
	match_start, match_end := re.match_string('')
	assert match_start == -1 && match_end == 0
	find_start, find_end := re.find('')
	assert find_start == -1 && find_end == -1
	assert re.find_all('') == []int{}
	assert re.find_all_str('') == []string{}
}

fn test_repeated_use_of_one_regex_object_is_stable() {
	mut re := regex.regex_opt(r'(\w+) (\w+)') or { panic(err) }
	first_start, first_end := re.match_string('ab cd')
	assert first_start == 0 && first_end == 5
	assert re.groups == [0, 2, 3, 5]
	second_start, second_end := re.match_string('ef gh')
	assert second_start == 0 && second_end == 5
	assert re.groups == [0, 2, 3, 5]
}
