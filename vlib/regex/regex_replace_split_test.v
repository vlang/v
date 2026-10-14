import regex

// One row of the replace tables: the pattern, the input, the replacement
// template, and the resulting string.
struct ReplaceCase {
	pattern string
	text    string
	repl    string
	want    string
	n       int
}

// `\0` in the template is the whole match, `\1`..`\9` are group 1..9, and a
// lone backslash is a literal one. An out of range group substitutes nothing.
const replace_cases = [
	ReplaceCase{
		pattern: r'(\d+)'
		text:    'abc 123 def'
		repl:    r'[\0]'
		want:    'abc [123] def'
	},
	ReplaceCase{
		pattern: r'(a\w) '
		text:    'Today is a good day.'
		repl:    r'[\0] '
		want:    'Tod[ay] is a good day.'
	},
	ReplaceCase{
		pattern: r'(a\w) '
		text:    'Today is a good day.'
		repl:    r'[\1] '
		want:    'Tod[] is a good day.'
	},
	ReplaceCase{
		pattern: r'(a)(\w) '
		text:    'Today is a good day.'
		repl:    r'[\0_\1] '
		want:    'Tod[a_y] is a good day.'
	},
	ReplaceCase{
		pattern: r'(a)\w '
		text:    'Today is a good day.'
		repl:    r'[\9] '
		want:    'Tod[] is a good day.'
	},
	ReplaceCase{
		pattern: r'\d'
		text:    'a1b2'
		repl:    r'\'
		want:    r'a\b\'
	},
	ReplaceCase{
		pattern: r'\d+'
		text:    'no digits here'
		repl:    '#'
		want:    'no digits here'
	},
]

// `replace_simple` is `replace` without template parsing, so `\0` stays
// literal text.
const replace_simple_cases = [
	ReplaceCase{
		pattern: r'(pi?(ba)+o)'
		text:    'oggi pibao di pbababao'
		repl:    'CIAO'
		want:    'oggi CIAO di CIAO'
	},
	ReplaceCase{
		pattern: r'[Tt]o\w+'
		text:    'Today and tomorrow.'
		repl:    'X'
		want:    'X and X.'
	},
	ReplaceCase{
		pattern: r'\d+'
		text:    'no digits'
		repl:    '#'
		want:    'no digits'
	},
	ReplaceCase{
		pattern: r'\d'
		text:    'a1b2'
		repl:    r'[\0]'
		want:    r'a[\0]b[\0]'
	},
]

// `replace_n` takes a positive count from the left, a negative one from the
// right, and 0 to do nothing. A count past the number of matches replaces all.
const replace_n_cases = [
	ReplaceCase{
		pattern: r'\d+'
		text:    'a 1 b 22 c 333'
		repl:    '#'
		want:    'a 1 b 22 c 333'
		n:       0
	},
	ReplaceCase{
		pattern: r'\d+'
		text:    'a 1 b 22 c 333'
		repl:    '#'
		want:    'a # b 22 c 333'
		n:       1
	},
	ReplaceCase{
		pattern: r'\d+'
		text:    'a 1 b 22 c 333'
		repl:    '#'
		want:    'a # b # c 333'
		n:       2
	},
	ReplaceCase{
		pattern: r'\d+'
		text:    'a 1 b 22 c 333'
		repl:    '#'
		want:    'a # b # c #'
		n:       3
	},
	ReplaceCase{
		pattern: r'\d+'
		text:    'a 1 b 22 c 333'
		repl:    '#'
		want:    'a # b # c #'
		n:       9
	},
	ReplaceCase{
		pattern: r'\d+'
		text:    'a 1 b 22 c 333'
		repl:    '#'
		want:    'a 1 b 22 c #'
		n:       -1
	},
	ReplaceCase{
		pattern: r'\d+'
		text:    'a 1 b 22 c 333'
		repl:    '#'
		want:    'a 1 b # c #'
		n:       -2
	},
	ReplaceCase{
		pattern: r'\d+'
		text:    'a 1 b 22 c 333'
		repl:    '#'
		want:    'a # b # c #'
		n:       -9
	},
]

// One row of the split tables: the pattern, the input, and the sections.
struct SplitCase {
	pattern string
	text    string
	want    []string
}

const split_cases = [
	SplitCase{
		pattern: r'-+'
		text:    'one--two---three'
		want:    ['one', 'two', 'three']
	},
	SplitCase{
		pattern: r','
		text:    'a,b,c'
		want:    ['a', 'b', 'c']
	},
	SplitCase{
		pattern: r'\s'
		text:    'a b c'
		want:    ['a', 'b', 'c']
	},
	SplitCase{
		pattern: r'\d'
		text:    '1234'
		want:    ['', '', '', '', '']
	},
	SplitCase{
		pattern: r'-'
		text:    '-a'
		want:    ['', 'a']
	},
	SplitCase{
		pattern: r'-'
		text:    'a-'
		want:    ['a', '']
	},
	SplitCase{
		pattern: r'-'
		text:    'a-b'
		want:    ['a', 'b']
	},
	// no match: the whole input is the only section
	SplitCase{
		pattern: r'\d'
		text:    'foobar'
		want:    ['foobar']
	},
	// a pattern that can only match the empty string splits per character
	SplitCase{
		pattern: r'-*'
		text:    'ab'
		want:    ['', 'a', 'b', '']
	},
]

const split_texts = ['', 'a', 'ab 12 cd', 'a,b,,c', 'abab', '   ', 'x--y', 'one two three']

fn test_replace_expands_group_references() {
	for c in replace_cases {
		re := regex.regex_opt(c.pattern) or { panic(err) }
		assert re.replace(c.text, c.repl) == c.want, 'pattern "${c.pattern}" repl "${c.repl}" on "${c.text}"'
	}
}

fn test_replace_simple_does_not_parse_templates() {
	for c in replace_simple_cases {
		mut re := regex.regex_opt(c.pattern) or { panic(err) }
		assert re.replace_simple(c.text, c.repl) == c.want, 'pattern "${c.pattern}" repl "${c.repl}" on "${c.text}"'
	}
}

fn test_replace_n_counts_from_either_end() {
	mut re := regex.regex_opt(r'\d+') or { panic(err) }
	for c in replace_n_cases {
		assert re.replace_n(c.text, c.repl, c.n) == c.want, 'pattern "${c.pattern}" count ${c.n}'
	}
}

fn test_replaces_over_the_same_sets_agree() {
	// with no group reference in the template, `replace` and `replace_simple`
	// must produce the same string
	for c in replace_simple_cases {
		mut re := regex.regex_opt(c.pattern) or { panic(err) }
		if c.repl.contains('\\') {
			continue
		}
		assert re.replace(c.text, c.repl) == re.replace_simple(c.text, c.repl), 'pattern "${c.pattern}"'
	}
}

// For a separator that cannot match the empty string, `split` must return
// exactly one more section than `find_all_str` found separators, and joining
// the sections back must give the input with the separators removed.
fn test_split_and_find_all_str_agree() {
	pairs := [
		SplitCase{r'-+', 'one--two---three', ['one', 'two', 'three']},
		SplitCase{r',', 'a,b,c', ['a', 'b', 'c']},
		SplitCase{r'\s', 'a b c', ['a', 'b', 'c']},
		SplitCase{r'\d', '1234', ['', '', '', '', '']},
		SplitCase{r'\d', 'foobar', ['foobar']},
	]
	for c in pairs {
		mut re := regex.regex_opt(c.pattern) or { panic(err) }
		sections := re.split(c.text)
		found := re.find_all_str(c.text)
		assert sections == c.want, 'pattern "${c.pattern}" on "${c.text}": ${sections}'
		assert sections.len == found.len + 1, 'pattern "${c.pattern}" on "${c.text}": ${sections.len} sections for ${found.len} separators'
		assert sections.join('') == c.want.join(''), 'pattern "${c.pattern}" on "${c.text}"'
	}
}

fn test_split_matches_the_expected_sections() {
	for c in split_cases {
		mut re := regex.regex_opt(c.pattern) or { panic(err) }
		assert re.split(c.text) == c.want, 'pattern "${c.pattern}" on "${c.text}"'
	}
}

// An empty input still splits: `find_all` is empty, so the single section is
// the empty string itself.
fn test_split_on_an_empty_input() {
	mut re := regex.regex_opt(r'\d+') or { panic(err) }
	assert re.split('') == ['']
	assert re.find_all('') == []int{}
}

fn test_split_on_text_without_the_separator_is_a_single_section() {
	for text in split_texts {
		mut re := regex.regex_opt(r'ZZZ') or { panic(err) }
		assert re.split(text) == [text], 'text "${text}"'
	}
}

// The callback receives the live regex and the match bounds, so it can read
// the captures recorded for the match it is being asked about, and fall back
// to the matched text when the pattern has no groups at all.
fn widen_repl(re regex.RE, in_txt string, start int, end int) string {
	group := re.get_group_by_id(in_txt, 0)
	return '[' + if group.len > 0 { group } else { in_txt[start..end] } + ']'
}

fn upper_repl(re regex.RE, in_txt string, start int, end int) string {
	group := re.get_group_by_id(in_txt, 0)
	return (if group.len > 0 { group } else { in_txt[start..end] }).to_upper()
}

// `replace_by_fn` gets the regex, the whole input and the match bounds, and
// its return value is spliced in for every non overlapping match.
fn test_replace_by_fn_uses_the_match_bounds() {
	mut words := regex.regex_opt(r'([a-z]+)') or { panic(err) }
	assert words.replace_by_fn('ab 12 cd', widen_repl) == '[ab] 12 [cd]'
	assert words.replace_by_fn('ab 12 cd', upper_repl) == 'AB 12 CD'
}

// With no match the callback is never called and the input is returned as is.
fn test_replace_by_fn_with_no_match_returns_the_input() {
	mut re := regex.regex_opt(r'\d+') or { panic(err) }
	assert re.replace_by_fn('abc', widen_repl) == 'abc'
	assert re.replace_by_fn('', widen_repl) == ''
}

// `replace` on an empty input never enters the loop, so the result is empty.
fn test_replace_on_an_empty_input() {
	re := regex.regex_opt(r'\d+') or { panic(err) }
	assert re.replace('', '#') == ''
}

// `replace` takes a shared receiver and clones the mutable state, so a
// constant compiled regex can be reused across calls without the second call
// seeing the first one's group data.
const shared_digits = regex.regex_opt(r'(\d+)')!

fn test_replace_on_a_const_regex_is_repeatable() {
	assert shared_digits.replace('abc 123 def', r'[\0]') == 'abc [123] def'
	assert shared_digits.replace('abc 123 def', r'[\0]') == 'abc [123] def'
	assert shared_digits.groups.len == 0
	assert shared_digits.replace('4 5 6', r'[\0]') == '[4] [5] [6]'
}
