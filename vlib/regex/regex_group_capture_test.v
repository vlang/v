import regex

// One row of the OR tables: the pattern, the text, and the (start, end) pair.
struct OrCase {
	pattern string
	text    string
	start   int
	end     int
}

// The OR operator works on single tokens, not on whole alternatives, so
// `abc|bde` reads as `ab` then `c|b` then `de`.
const or_token_cases = [
	OrCase{r'abc|bde', 'abcde', 0, 5},
	OrCase{r'a|b', 'b', 0, 1},
	OrCase{r'a|b', 'c', -1, 0},
]

const grouped_or_cases = [
	OrCase{r'^((cat)|(dog))$', 'cat', 0, 3},
	OrCase{r'^((cat)|(dog))$', 'dog', 0, 3},
	OrCase{r'^((cat)|(dog))$', 'catdog', -1, 0},
	OrCase{r'^((cat)|(dog))$', 'cow', -1, 0},
	OrCase{r'((a)|(b))+', 'abab', 0, 4},
	OrCase{r'((a)|(b))+', 'a', 0, 1},
]

// One row of the capture table: the pattern, the text, and the flat
// `groups` array `[start0, end0, start1, end1, ...]`.
struct CaptureCase {
	pattern string
	text    string
	start   int
	end     int
	groups  []int
}

const capture_cases = [
	CaptureCase{
		pattern: r'(\w+)@(\w+)'
		text:    'user@host'
		start:   0
		end:     9
		groups:  [int(0), 4, 5, 9]
	},
	CaptureCase{
		pattern: r'(?:a)(b)'
		text:    'ab'
		start:   0
		end:     2
		groups:  [int(1), 2]
	},
]

// The same table, but driven through `find`, which is free to start further
// into the text.
const capture_find_cases = [
	CaptureCase{
		pattern: r'(b+)(c+)'
		text:    'aabbbccccdd'
		start:   2
		end:     9
		groups:  [int(2), 5, 5, 9]
	},
]

// A quantified group whose body is a bare OR never matches: the group_rep
// bookkeeping is only reached through the OR's jump table.
// NOTE: this is a limitation, not a documented rule. The workaround is to wrap
// each branch in its own group, as `((a)|(b))+` does.
const quantified_bare_or_cases = [r'(a|b)+', r'(a|b)*', r'(?:a|b)+', r'(?:a|b)*']

fn test_or_operator_works_on_single_tokens() {
	for c in or_token_cases {
		mut re := regex.regex_opt(c.pattern) or { panic(err) }
		start, end := re.match_string(c.text)
		msg := 'pattern "${c.pattern}" on "${c.text}"'
		assert start == c.start, '${msg}: start ${start}, want ${c.start}'
		assert end == c.end, '${msg}: end ${end}, want ${c.end}'
	}
}

fn test_or_inside_groups_selects_whole_alternatives() {
	for c in grouped_or_cases {
		mut re := regex.regex_opt(c.pattern) or { panic(err) }
		start, end := re.match_string(c.text)
		msg := 'pattern "${c.pattern}" on "${c.text}"'
		assert start == c.start, '${msg}: start ${start}, want ${c.start}'
		assert end == c.end, '${msg}: end ${end}, want ${c.end}'
	}
}

fn test_or_between_two_char_classes_is_rejected() {
	for pattern in [r'[a]|[b]', r'([a]|[b])*', r'x[a]|[b]y'] {
		_, re_err, _ := regex.regex_base(pattern)
		assert re_err == regex.err_invalid_or_with_cc, 'pattern "${pattern}" gave ${re_err}'
	}
}

fn test_capturing_groups_report_their_bounds() {
	for c in capture_cases {
		mut re := regex.regex_opt(c.pattern) or { panic(err) }
		start, end := re.match_string(c.text)
		msg := 'pattern "${c.pattern}" on "${c.text}"'
		assert start == c.start, '${msg}: start ${start}, want ${c.start}'
		assert end == c.end, '${msg}: end ${end}, want ${c.end}'
		assert re.groups == c.groups, '${msg}: groups ${re.groups}, want ${c.groups}'
	}
}

fn test_capturing_groups_follow_a_find_offset() {
	for c in capture_find_cases {
		mut re := regex.regex_opt(c.pattern) or { panic(err) }
		start, end := re.find(c.text)
		msg := 'pattern "${c.pattern}" on "${c.text}"'
		assert start == c.start, '${msg}: start ${start}, want ${c.start}'
		assert end == c.end, '${msg}: end ${end}, want ${c.end}'
		assert re.groups == c.groups, '${msg}: groups ${re.groups}, want ${c.groups}'
	}
}

fn test_group_count_and_ids() {
	mut two := regex.regex_opt(r'(a)(b)') or { panic(err) }
	assert two.group_count == 2
	// `groups` is allocated by the first match, not by the compiler
	assert two.groups.len == 0
	two.match_string('ab')
	assert two.groups.len == 4

	// a non capturing group does not consume an id
	mut one := regex.regex_opt(r'(?:a)(b)') or { panic(err) }
	assert one.group_count == 1

	// no groups at all
	mut plain := regex.regex_opt(r'abc') or { panic(err) }
	assert plain.group_count == 0
	assert plain.groups.len == 0
	assert plain.get_group_list() == []regex.Re_group{}
}

fn test_quantified_group_with_a_bare_or_never_matches() {
	for pattern in quantified_bare_or_cases {
		mut re := regex.regex_opt(pattern) or { panic(err) }
		assert !re.matches_string('abab'), 'pattern "${pattern}" unexpectedly matched "abab"'
		start, _ := re.match_string('abab')
		assert start == regex.no_match_found, 'pattern "${pattern}" gave start ${start}'
	}
}

fn test_named_groups_are_addressable_by_name() {
	mut re := regex.regex_opt(r'(?P<user>\w+)@(?P<host>\w+)') or { panic(err) }
	start, end := re.match_string('user@host')
	assert start == 0 && end == 9
	assert re.group_map == {
		'user': int(1)
		'host': int(2)
	}
	assert re.get_group_by_name('user@host', 'user') == 'user'
	assert re.get_group_by_name('user@host', 'host') == 'host'
	user_start, user_end := re.get_group_bounds_by_name('user')
	assert user_start == 0 && user_end == 4
	host_start, host_end := re.get_group_bounds_by_name('host')
	assert host_start == 5 && host_end == 9
}

fn test_unknown_group_names_and_ids_report_nothing() {
	mut re := regex.regex_opt(r'(?P<a>\w+)@(?P<b>\w+)') or { panic(err) }
	re.match_string('user@host')
	assert re.get_group_by_name('user@host', 'missing') == ''
	missing_start, missing_end := re.get_group_bounds_by_name('missing')
	assert missing_start == -1 && missing_end == -1
	assert re.get_group_by_id('user@host', 9) == ''
	oob_start, oob_end := re.get_group_bounds_by_id(9)
	assert oob_start == -1 && oob_end == -1
	assert re.get_group_by_id('user@host', 0) == 'user'
	assert re.get_group_by_id('user@host', 1) == 'host'
	id_start, id_end := re.get_group_bounds_by_id(0)
	assert id_start == 0 && id_end == 4
}

// The same name used twice recycles one id instead of allocating a second.
fn test_a_repeated_group_name_recycles_its_id() {
	mut re := regex.regex_opt(r'(?P<x>a)|(?P<x>b)') or { panic(err) }
	start, end := re.match_string('b')
	assert start == 0 && end == 1
	assert re.group_count == 1
	assert re.group_map == {
		'x': int(1)
	}
	assert re.get_group_by_name('b', 'x') == 'b'
}

fn test_get_group_list_matches_the_groups_array() {
	mut re := regex.regex_opt(r'(\w+) (\w+)') or { panic(err) }
	re.match_string('ab cd')
	assert re.get_group_list() == [
		regex.Re_group{
			start: 0
			end:   2
		},
		regex.Re_group{
			start: 3
			end:   5
		},
	]
}

// NOTE: a failed match does not clear the groups it already filled. The
// end-of-text handler stores whatever the first group had reached before the
// match collapsed, so `(\w+)@(\w+)` on "zzz" leaves group 0 as [0, 3] even
// though the overall match failed.
fn test_a_failed_match_keeps_the_groups_it_filled() {
	mut re := regex.regex_opt(r'(\w+)@(\w+)') or { panic(err) }
	start, _ := re.match_string('zzz')
	assert start == regex.no_match_found
	assert re.groups == [int(0), 3, -1, -1]
	assert re.get_group_by_id('zzz', 0) == 'zzz'
	kept_start, kept_end := re.get_group_bounds_by_id(0)
	assert kept_start == 0 && kept_end == 3
	assert re.get_group_by_id('zzz', 1) == ''
}

// A fresh regex has no group storage at all: `groups` is allocated by the
// first match.
// NOTE: `get_group_bounds_by_id` only guards on `group_count`, not on
// `groups.len`, so calling it before the first match indexes an empty array
// and panics. `get_group_by_id` guards on the values and is safe.
fn test_groups_are_allocated_by_the_first_match() {
	mut re := regex.regex_opt(r'(a)(b)') or { panic(err) }
	assert re.groups.len == 0
	assert re.get_group_by_id('ab', 0) == ''
	assert re.get_group_by_id('ab', 1) == ''
	assert re.get_group_list() == []regex.Re_group{}
}

fn test_negation_group_blocks_a_matching_prefix() {
	mut re := regex.regex_opt(r'(?!auto)\w+le') or { panic(err) }
	assert !re.matches_string('automobile')
	assert re.matches_string('botomobile')
	assert re.matches_string('moto_mobile')
	assert !re.matches_string('auto_caravan')

	mut simple := regex.regex_opt(r'(?!ab)\w+') or { panic(err) }
	assert !simple.matches_string('abcd')
	start, end := simple.match_string('xabcd')
	assert start == 0 && end == 5
}

fn test_nested_groups_are_numbered_outside_in() {
	mut re := regex.regex_opt(r'((b+).*)(d+)') or { panic(err) }
	start, end := re.find('aabbbccccdd')
	assert start == 2 && end == 11
	assert re.groups == [int(2), 9, 2, 5, 9, 11]
	assert re.get_group_by_id('aabbbccccdd', 0) == 'bbbcccc'
	assert re.get_group_by_id('aabbbccccdd', 1) == 'bbb'
	assert re.get_group_by_id('aabbbccccdd', 2) == 'dd'
}
