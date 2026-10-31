import regex

// One row of the character-class tables: a fully anchored pattern, the text it
// runs against, and whether it should match.
struct ClassCase {
	pattern string
	text    string
	matches bool
}

const metachar_cases = [
	ClassCase{r'^\d+$', '123', true},
	ClassCase{r'^\d+$', '12a', false},
	ClassCase{r'^\d+$', '', false},
	ClassCase{r'^\D+$', 'abc', true},
	ClassCase{r'^\D+$', 'a1', false},
	ClassCase{r'^\w+$', 'a_1', true},
	ClassCase{r'^\w+$', 'a-1', false},
	ClassCase{r'^\W+$', '!@#', true},
	ClassCase{r'^\W+$', 'a!', false},
	ClassCase{r'^\s+$', ' \t\n', true},
	ClassCase{r'^\s+$', ' a', false},
	ClassCase{r'^\S+$', 'abc', true},
	ClassCase{r'^\S+$', 'a b', false},
	ClassCase{r'^\a+$', 'abc', true},
	ClassCase{r'^\a+$', 'aBc', false},
	ClassCase{r'^\A+$', 'ABC', true},
	ClassCase{r'^\A+$', 'aBC', false},
]

const class_body_cases = [
	ClassCase{r'^[abc]+$', 'abcabc', true},
	ClassCase{r'^[abc]+$', 'abd', false},
	ClassCase{r'^[a-z]+$', 'abc', true},
	ClassCase{r'^[a-z]+$', 'aB', false},
	ClassCase{r'^[a-zA-Z0-9]+$', 'aZ9', true},
	ClassCase{r'^[a-zA-Z0-9]+$', 'aZ9_', false},
	ClassCase{r'^[^0-9]+$', 'abc', true},
	ClassCase{r'^[^0-9]+$', 'a1', false},
	ClassCase{r'^[a-z-]+$', 'a-b', true},
	ClassCase{r'^[-a-z]+$', '-a', true},
	ClassCase{r'^[\d]+$', '123', true},
	ClassCase{r'^[\w]+$', 'a_1', true},
	ClassCase{r'^[\s]+$', ' ', true},
	ClassCase{r'^[\S]+$', 'a', true},
	ClassCase{r'^[^\d]+$', 'abc', true},
	ClassCase{r'^[^\d]+$', '1', false},
	ClassCase{r'^[^\s]+$', 'a', true},
	ClassCase{r'^[\w-]+$', 'a-b', true},
	ClassCase{r'^[a-z\-]+$', 'a-', true},
	ClassCase{r'^[a-z\-]+$', 'a_b', false},
]

const escaped_cases = [
	ClassCase{r'^\.+$', '..', true},
	ClassCase{r'^\.+$', 'ab', false},
	ClassCase{r'^\*+$', '**', true},
	ClassCase{r'^\++$', '++', true},
	ClassCase{r'^\?+$', '??', true},
	ClassCase{r'^\(+$', '((', true},
	ClassCase{r'^\)+$', '))', true},
	ClassCase{r'^\[+$', '[[', true},
	ClassCase{r'^\]+$', ']]', true},
	ClassCase{r'^\{+$', '{{', true},
	ClassCase{r'^\}+$', '}}', true},
	ClassCase{r'^\|+$', '||', true},
	ClassCase{r'^\^+$', '^^', true},
	ClassCase{r'^\!+$', '!!', true},
	ClassCase{r'^\:+$', '::', true},
	ClassCase{r'^\-+$', '--', true},
	ClassCase{r'^\\+$', '\\\\', true},
]

const unescaped_cases = [
	ClassCase{r'^.+$', '..', true},
	ClassCase{r'^a*b$', 'b', true},
]

fn test_metachars_match_their_character_sets() {
	for c in metachar_cases {
		mut re := regex.regex_opt(c.pattern) or { panic(err) }
		assert re.matches_string(c.text) == c.matches, 'pattern "${c.pattern}" on "${c.text}": want ${c.matches}'
	}
}

fn test_char_class_bodies_match_their_listed_characters() {
	for c in class_body_cases {
		mut re := regex.regex_opt(c.pattern) or { panic(err) }
		assert re.matches_string(c.text) == c.matches, 'pattern "${c.pattern}" on "${c.text}": want ${c.matches}'
	}
}

fn test_backslash_escapes_a_meta_character() {
	for c in escaped_cases {
		mut re := regex.regex_opt(c.pattern) or { panic(err) }
		assert re.matches_string(c.text) == c.matches, 'pattern "${c.pattern}" on "${c.text}": want ${c.matches}'
	}
}

fn test_unescaped_meta_characters_keep_their_special_meaning() {
	for c in unescaped_cases {
		mut re := regex.regex_opt(c.pattern) or { panic(err) }
		assert re.matches_string(c.text) == c.matches, 'pattern "${c.pattern}" on "${c.text}": want ${c.matches}'
	}
}

// A negated class steps over the newline, because the module has no "dotall"
// split: both `.` and `[^x]` match `\n`.
//
// NOTE: `\n`, `\t`, `\r`, `\v` and `\f` are not escapes in this engine. Outside
// a class they are `err_syntax_error`; inside one the backslash is dropped and
// only the letter remains, so `[^\n]` means "not the letter n" and happily
// matches the newline itself.
fn test_negated_char_class_matches_a_newline() {
	mut any := regex.regex_opt(r'[^a]') or { panic(err) }
	start, end := any.match_string('\n')
	assert start == 0 && end == 1

	mut run := regex.regex_opt(r'[^a]+') or { panic(err) }
	run_start, run_end := run.match_string('b\nb')
	assert run_start == 0 && run_end == 3

	mut not_letter_n := regex.regex_opt(r'[^\n]+') or { panic(err) }
	nn_start, nn_end := not_letter_n.match_string('ab\ncd')
	assert nn_start == 0 && nn_end == 5
}

fn test_newline_and_tab_escapes_are_not_supported() {
	for pattern in [r'\n', r'\t', r'\r', r'\v', r'\f', r'\0'] {
		_, re_err, _ := regex.regex_base(pattern)
		assert re_err == regex.err_syntax_error, 'pattern "${pattern}" gave ${re_err}'
	}
	// the class form keeps only the letter
	mut letter := regex.regex_opt(r'[\n]') or { panic(err) }
	assert letter.matches_string('n')
	assert !letter.matches_string('\n')
}

// `\s` is the fixed western set from the module constants, so a non-breaking
// space is not a space here.
fn test_space_metachar_is_the_ascii_space_set() {
	spaces := [' ', '\t', '\n', '\r', '\v', '\f']
	for ch in spaces {
		mut re := regex.regex_opt(r'^\s+$') or { panic(err) }
		assert re.matches_string(ch), 'expected "${ch.bytes()}" to be a space'
	}
	mut re := regex.regex_opt(r'^\s+$') or { panic(err) }
	assert !re.matches_string('\u00a0')
	assert !re.matches_string('x')
}

fn test_char_class_ranges_cross_ascii_boundaries() {
	mut digits := regex.regex_opt(r'^[0-9]+$') or { panic(err) }
	assert digits.matches_string('0123456789')
	assert !digits.matches_string('0123456789a')

	mut alnum := regex.regex_opt(r'^[0-9a-fA-F]+$') or { panic(err) }
	assert alnum.matches_string('deadBEEF09')
	assert !alnum.matches_string('deadBEEF09g')

	mut punct := regex.regex_opt(r'^[!-/]+$') or { panic(err) }
	assert punct.matches_string('!"#$%&\'()*+,-./')
	assert !punct.matches_string('!"/a')
}

// NOTE: `\x`.. and `\X`.. escapes are only parsed as bytes outside a class.
// Inside one, the backslash is dropped and `x`, the digits and the `-` become
// literal class members, so `[\x41-\x43]` is the set `{x, 4, 1}` plus the
// range `1`..`\` and does match `ABCD` for reasons unrelated to hex.
fn test_hex_escapes_are_not_parsed_inside_a_char_class() {
	mut hex := regex.regex_opt(r'^[\x41]+$') or { panic(err) }
	assert !hex.matches_string('A')
	assert hex.matches_string('x41')

	mut hex_range := regex.regex_opt(r'^[\x41-\x43]+$') or { panic(err) }
	assert hex_range.matches_string('ABCD')
}

fn test_unterminated_char_class_is_a_syntax_error() {
	for pattern in [r'[]', r'[]]', r'[^]', r'[a', r'[abc', r'[\d'] {
		_, re_err, _ := regex.regex_base(pattern)
		assert re_err == regex.err_syntax_error, 'pattern "${pattern}" gave ${re_err}'
	}
}
