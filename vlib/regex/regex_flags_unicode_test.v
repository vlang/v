import regex

// One row of the flag tables: the pattern, the text, and the (start, end)
// pair, all run with the named flag set.
struct FlagCase {
	pattern string
	text    string
	start   int
	end     int
}

// One row of the case folding table: the same fields plus the start index the
// pattern reports *without* the flag, so the flag is proven to be what changed
// the answer.
struct CaseFoldCase {
	pattern     string
	text        string
	start       int
	end         int
	plain_start int
}

// `f_nl` stops the match at the first newline, which the default (stop at end
// of string) does not.
const new_line_flag_cases = [
	FlagCase{r'a.c', 'a\nc', -1, 1},
	FlagCase{r'\w+', 'ab\ncd', 0, 2},
	FlagCase{r'\w+', '\nab', -1, 0},
	FlagCase{r'abc', 'abc', 0, 3},
]

// `f_bin` ignores the utf-8 decoding, so a multibyte character is no longer a
// single token.
const binary_flag_cases = [
	FlagCase{'é', 'é', 0, 2},
	FlagCase{'aé', 'aé', 0, 3},
]

// `f_efm` returns as soon as the first token of the query matches.
const first_match_flag_cases = [
	FlagCase{r'abc', 'abc', 1, 2},
	FlagCase{r'\d+', '123', 1, 2},
]

// `f_ci` folds ASCII case on simple chars, on char classes and on the
// backslash validators alike.
const case_insensitive_cases = [
	CaseFoldCase{
		pattern:     r'hello'
		text:        'HeLLo'
		start:       0
		end:         5
		plain_start: -1
	},
	CaseFoldCase{
		pattern:     r'^[A-Z]+$'
		text:        'abcXYZ'
		start:       0
		end:         6
		plain_start: -1
	},
	CaseFoldCase{
		pattern:     r'^\a+$'
		text:        'AbC'
		start:       0
		end:         3
		plain_start: -1
	},
	CaseFoldCase{
		pattern:     r'^[a-f]+$'
		text:        'ABCdef'
		start:       0
		end:         6
		plain_start: -1
	},
	CaseFoldCase{
		pattern:     r'^[^a]+$'
		text:        'B'
		start:       0
		end:         1
		plain_start: 0
	},
]

fn test_dot_matches_a_newline_because_there_is_no_dotall_flag() {
	mut re := regex.regex_opt(r'a.c') or { panic(err) }
	start, end := re.match_string('a\nc')
	assert start == 0 && end == 3

	// the same, through a group and a star
	mut star := regex.regex_opt(r'^a(.*)b$') or { panic(err) }
	star_start, star_end := star.match_string('a\nb')
	assert star_start == 0 && star_end == 3
}

fn test_consecutive_dots_are_a_syntax_error() {
	for pattern in [r'..', r'a..b', r'...', r'.{2}.', r'.*.', r'.?.'] {
		_, re_err, _ := regex.regex_base(pattern)
		assert re_err == regex.err_consecutive_dots, 'pattern "${pattern}" gave ${re_err}'
	}
}

fn test_new_line_flag_stops_the_match_at_a_newline() {
	for c in new_line_flag_cases {
		mut re := regex.regex_opt(c.pattern) or { panic(err) }
		re.flag |= regex.f_nl
		start, end := re.match_string(c.text)
		msg := 'pattern "${c.pattern}" on "${c.text}" with f_nl'
		assert start == c.start, '${msg}: start ${start}, want ${c.start}'
		assert end == c.end, '${msg}: end ${end}, want ${c.end}'
	}

	// the same cases without the flag run on to the end of the string
	mut plain := regex.regex_opt(r'a.c') or { panic(err) }
	plain_start, plain_end := plain.match_string('a\nc')
	assert plain_start == 0 && plain_end == 3
}

fn test_binary_flag_treats_a_multibyte_char_as_bytes() {
	for c in binary_flag_cases {
		mut utf8 := regex.regex_opt(c.pattern) or { panic(err) }
		utf8_start, utf8_end := utf8.match_string(c.text)
		assert utf8_start == c.start, 'utf8 pattern "${c.pattern}" on "${c.text}"'
		assert utf8_end == c.end, 'utf8 pattern "${c.pattern}" on "${c.text}"'
	}
	mut bin := regex.regex_opt('é') or { panic(err) }
	bin.flag |= regex.f_bin
	bin_start, bin_end := bin.match_string('é')
	assert bin_start == regex.no_match_found, 'got start ${bin_start}'
	assert bin_end == 0
}

fn test_first_match_flag_exits_on_the_first_token() {
	for c in first_match_flag_cases {
		mut re := regex.regex_opt(c.pattern) or { panic(err) }
		re.flag |= regex.f_efm
		start, end := re.match_string(c.text)
		msg := 'pattern "${c.pattern}" on "${c.text}" with f_efm'
		assert start == c.start, '${msg}: start ${start}, want ${c.start}'
		assert end == c.end, '${msg}: end ${end}, want ${c.end}'
	}
}

fn test_case_insensitive_flag_folds_ascii_case() {
	for c in case_insensitive_cases {
		mut re := regex.regex_opt(c.pattern) or { panic(err) }
		re.flag |= regex.f_ci
		start, end := re.match_string(c.text)
		msg := 'pattern "${c.pattern}" on "${c.text}" with f_ci'
		assert start == c.start, '${msg}: start ${start}, want ${c.start}'
		assert end == c.end, '${msg}: end ${end}, want ${c.end}'

		mut plain := regex.regex_opt(c.pattern) or { panic(err) }
		plain_start, _ := plain.match_string(c.text)
		assert plain_start == c.plain_start, '${msg}: without the flag the start was ${plain_start}, want ${c.plain_start}'
	}
}

// A case folded pattern must accept the lower and the upper spelling of the
// same word, which is what matching the lowercased pattern by hand gives.
fn test_case_insensitive_matching_equals_the_lowercased_pattern() {
	words := ['Hello', 'WORLD', 'MiXeD', 'abc', 'ABC', 'xYz', 'AaBbCc']
	for word in words {
		mut folded := regex.regex_opt(word) or { panic(err) }
		folded.flag |= regex.f_ci
		mut lowered := regex.regex_opt(word.to_lower()) or { panic(err) }
		mut uppered := regex.regex_opt(word.to_upper()) or { panic(err) }
		assert folded.matches_string(word.to_lower()) == lowered.matches_string(word.to_lower()), 'word "${word}": folded and lowercased disagree on the lower spelling'
		assert folded.matches_string(word.to_upper()) == uppered.matches_string(word.to_upper()), 'word "${word}": folded and uppercased disagree on the upper spelling'
	}
}

// The backslash validators are ASCII only, so a non ASCII letter is not a
// word char here even though the dot matches it as one token.
fn test_word_chars_are_ascii_only() {
	mut re := regex.regex_opt(r'\w+') or { panic(err) }
	start, end := re.match_string('héllo wörld')
	assert start == 0 && end == 1

	mut cls := regex.regex_opt(r'[a-z]+') or { panic(err) }
	cls_start, cls_end := cls.match_string('café')
	assert cls_start == 0 && cls_end == 3

	mut dot := regex.regex_opt(r'.+') or { panic(err) }
	dot_start, dot_end := dot.match_string('aé')
	assert dot_start == 0 && dot_end == 3
}

// A multibyte character is a single token: the pattern advances by its full
// byte length, and a `find` reports byte offsets.
fn test_a_multibyte_char_is_one_token() {
	mut re := regex.regex_opt('é') or { panic(err) }
	start, end := re.find('xéy')
	assert start == 1 && end == 3

	mut cls := regex.regex_opt('[é]') or { panic(err) }
	cls_start, cls_end := cls.find('xéy')
	assert cls_start == 1 && cls_end == 3

	mut range := regex.regex_opt('[à-ÿ]+') or { panic(err) }
	r_start, r_end := range.match_string('àáâ abc')
	assert r_start == 0 && r_end == 6
}

// `get_query` rebuilds the query from the compiled program. It is idempotent:
// compiling its output gives the same program back.
fn test_get_query_round_trips() {
	patterns := [r'abc', r'a+b*c?', r'a{2,4}?', r'[a-z]+', r'^a$', r'(a)(?:b)(?!c)', r'\d\w\s',
		r'a|b|c', r'[^x]', r'.', r'\x41', r'(?P<n>ab)+', r'[a-z\-]+']
	for pattern in patterns {
		mut first := regex.regex_opt(pattern) or { panic(err) }
		query := first.get_query()
		mut second := regex.regex_opt(query) or {
			assert false, 'pattern "${pattern}": rebuilt query "${query}" does not compile'
			continue
		}
		assert second.get_query() == query, 'pattern "${pattern}": rebuilt query "${query}" is not stable'
	}
}

// `get_code` dumps the compiled program: one line per token, ending in a
// PROG_END instruction.
fn test_get_code_dumps_the_compiled_program() {
	mut re := regex.regex_opt(r'a+b') or { panic(err) }
	code := re.get_code()
	assert code.contains('v RegEx compiler v ${regex.v_regex_version} output:')
	assert code.contains('ist: 7fffffff [a]      query_ch {  1,MAX}')
	assert code.contains('ist: 7fffffff [b]      query_ch {  1,  1}')
	assert code.contains('ist: 88000000 PROG_END {  0,  0}')
	assert code.ends_with('========================================\n')
}

// With `debug` set, `get_query` prefixes each capturing group with its id and
// marks a non capturing one with `#-1`.
fn test_get_query_prints_group_ids_when_debugging() {
	mut re := regex.regex_opt(r'(a)(?:b)') or { panic(err) }
	assert re.get_query() == '(a)(?:b)'
	re.debug = 1
	assert re.get_query() == '#0(a)#-1(?:b)'
	re.debug = 0
	assert re.get_query() == '(a)(?:b)'
}
