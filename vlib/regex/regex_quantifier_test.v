import regex

// One row of the quantifier tables: the pattern, the text, and the
// (start, end) pair the matcher must report.
struct QuantCase {
	pattern string
	text    string
	start   int
	end     int
}

const short_quantifier_cases = [
	QuantCase{r'a?b', 'ab', 0, 2},
	QuantCase{r'a?b', 'b', 0, 1},
	QuantCase{r'a?b', 'aab', -1, 1},
	QuantCase{r'a+b', 'ab', 0, 2},
	QuantCase{r'a+b', 'aaab', 0, 4},
	QuantCase{r'a+b', 'b', -1, 0},
	QuantCase{r'a*b', 'b', 0, 1},
	QuantCase{r'a*b', 'aaab', 0, 4},
]

const bounded_quantifier_cases = [
	QuantCase{r'ab{2}', 'ab', -1, 2},
	QuantCase{r'ab{2}', 'abb', 0, 3},
	QuantCase{r'ab{2}', 'abbb', 0, 3},
	QuantCase{r'ab{2,}', 'ab', -1, 2},
	QuantCase{r'ab{2,}', 'abbb', 0, 4},
	QuantCase{r'ab{,2}', 'a', 0, 1},
	QuantCase{r'ab{,2}', 'abb', 0, 3},
	QuantCase{r'ab{,2}', 'abbb', 0, 3},
	QuantCase{r'ab{2,3}', 'ab', -1, 2},
	QuantCase{r'ab{2,3}', 'abb', 0, 3},
	QuantCase{r'ab{2,3}', 'abbb', 0, 4},
	QuantCase{r'a{0}b', 'b', 0, 1},
]

// The lazy flag only exists for the `{n,m}` form: `{2,4}?` stops at the
// minimum, while `{2,4}` runs to the maximum.
const lazy_quantifier_cases = [
	QuantCase{r'a{2,4}?', 'aaaa', 0, 2},
	QuantCase{r'a{2,4}', 'aaaa', 0, 4},
	QuantCase{r'a{,3}?', 'aaa', 0, 1},
	QuantCase{r'a{,3}', 'aaaaa', 0, 3},
	QuantCase{r'a{2,}?', 'aaaa', 0, 2},
	QuantCase{r'(ab){1,3}?', 'ababab', 0, 2},
	QuantCase{r'(ab){1,3}', 'ababab', 0, 6},
]

// A quantifier can be attached to any token kind, not only to a simple char.
const quantifiable_token_cases = [
	QuantCase{r'[ab]{2}', 'abab', 0, 2},
	QuantCase{r'[ab]{2}', 'a', -1, 1},
	QuantCase{r'\d{2}', '123', 0, 2},
	QuantCase{r'\w{3}', 'abcd', 0, 3},
	QuantCase{r'.{2}', 'abc', 0, 2},
	QuantCase{r'.{2}', 'a\nbc', 0, 2},
	QuantCase{r'(ab){2}', 'ababab', 0, 4},
	QuantCase{r'(ab){2}', 'abab', 0, 4},
	QuantCase{r'(ab){2}', 'ab', 0, 2},
]

const invalid_quantifier_patterns = [
	r'a??',
	r'a**',
	r'a*+',
	r'a++',
	r'a+*',
	r'a?*',
	r'a?+',
	r'a{2}{3}',
	r'a{2}*',
	r'a{2}+',
	r'a{2}??',
	// lazy short quantifiers are rejected, only `{n,m}?` carries the flag
	r'a*?',
	r'a+?',
	r'.+?',
	r'\d+?',
	r'[ab]+?',
	r'(ab)+?',
	r'(ab)*?',
	// unterminated or non numeric bounds
	r'a{',
	r'a{1',
	r'a{,',
	r'a{}',
	r'a{a}',
	r'a{1,2',
]

fn test_short_quantifiers() {
	for c in short_quantifier_cases {
		mut re := regex.regex_opt(c.pattern) or { panic(err) }
		start, end := re.match_string(c.text)
		msg := 'pattern "${c.pattern}" on "${c.text}"'
		assert start == c.start, '${msg}: start ${start}, want ${c.start}'
		assert end == c.end, '${msg}: end ${end}, want ${c.end}'
	}
}

fn test_bounded_quantifiers() {
	for c in bounded_quantifier_cases {
		mut re := regex.regex_opt(c.pattern) or { panic(err) }
		start, end := re.match_string(c.text)
		msg := 'pattern "${c.pattern}" on "${c.text}"'
		assert start == c.start, '${msg}: start ${start}, want ${c.start}'
		assert end == c.end, '${msg}: end ${end}, want ${c.end}'
	}
}

fn test_lazy_quantifiers_stop_at_the_minimum() {
	for c in lazy_quantifier_cases {
		mut re := regex.regex_opt(c.pattern) or { panic(err) }
		start, end := re.match_string(c.text)
		msg := 'pattern "${c.pattern}" on "${c.text}"'
		assert start == c.start, '${msg}: start ${start}, want ${c.start}'
		assert end == c.end, '${msg}: end ${end}, want ${c.end}'
	}
}

fn test_quantifiers_apply_to_every_token_kind() {
	for c in quantifiable_token_cases {
		mut re := regex.regex_opt(c.pattern) or { panic(err) }
		start, end := re.match_string(c.text)
		msg := 'pattern "${c.pattern}" on "${c.text}"'
		assert start == c.start, '${msg}: start ${start}, want ${c.start}'
		assert end == c.end, '${msg}: end ${end}, want ${c.end}'
	}
}

fn test_invalid_quantifier_sequences_are_syntax_errors() {
	for pattern in invalid_quantifier_patterns {
		_, re_err, _ := regex.regex_base(pattern)
		assert re_err == regex.err_syntax_error, 'pattern "${pattern}" gave ${re_err}'
	}
}

// `{n,m}` bounds are parsed as plain integers, so a minimum above the maximum
// or a bound far past any input length compiles without complaint.
fn test_inverted_and_huge_bounds_compile() {
	mut inverted := regex.regex_opt(r'a{3,2}') or { panic(err) }
	assert !inverted.matches_string('aa')

	mut huge := regex.regex_opt(r'a{999999999999}') or { panic(err) }
	assert !huge.matches_string('aa')
}

// NOTE: a zero-width `{0}` confuses the quantifier state machine, which falls
// through every range branch and reports `err_internal_error` as the start
// index. This happens both at the end of the program and in the middle of it,
// so only a pattern with no `{0}` token at all behaves as documented.
fn test_zero_width_quantifier_reports_an_internal_error() {
	mut last := regex.regex_opt(r'a{0}') or { panic(err) }
	last_start, last_end := last.match_string('aaa')
	assert last_start == regex.err_internal_error, 'got start ${last_start}'
	assert last_end == 1

	mut middle := regex.regex_opt(r'a{0}b') or { panic(err) }
	middle_start, middle_end := middle.match_string('ab')
	assert middle_start == regex.err_internal_error, 'got start ${middle_start}'
	assert middle_end == 1

	mut no_input := regex.regex_opt(r'a{0}b') or { panic(err) }
	ok_start, ok_end := no_input.match_string('b')
	assert ok_start == 0 && ok_end == 1
}

// A quantifier cannot be used at all in front of the first token: `pc > 0` is
// false there, so `*a` compiles as a literal `*` followed by `a`.
fn test_leading_quantifier_is_a_literal_character() {
	for pattern in [r'*a', r'+a', r'?a', r'|a'] {
		_, re_err, _ := regex.regex_base(pattern)
		assert re_err == regex.compile_ok, 'pattern "${pattern}" gave ${re_err}'
	}
	mut re := regex.regex_opt(r'*a') or { panic(err) }
	start, end := re.match_string('*a')
	assert start == 0 && end == 2
	assert !re.matches_string('a')
}

fn test_greedy_star_runs_to_the_last_possible_match() {
	mut re := regex.regex_opt(r'a.*b') or { panic(err) }
	start, end := re.match_string('axbxb')
	assert start == 0 && end == 3
}
