import regex

struct AlternationBoundaryCase {
	pattern string
	input   string
	start   int
	end     int
}

fn test_alternation_repetition_boundaries_and_utf8() {
	cases := [
		AlternationBoundaryCase{'a|b|c|d', 'xxdyy', 2, 3},
		AlternationBoundaryCase{'(a|b){2,3}', 'abac', 0, 3},
		AlternationBoundaryCase{'(a|b)*z', 'z', 0, 1},
		AlternationBoundaryCase{'(a|b)*z', 'abaz', 0, 4},
		// Reluctant repetition keeps the existing minimum-only continuation behavior.
		AlternationBoundaryCase{'(a|b){1,3}?z', 'abz', 1, 3},
		AlternationBoundaryCase{'(é|😀)+', 'é😀x', 0, 6},
		AlternationBoundaryCase{'((ab)|(cd)){2}', 'abcdx', 0, 4},
		AlternationBoundaryCase{'((ab)|(cd)){2,3}', 'abcdabx', 0, 6},
	]
	for item in cases {
		mut re := regex.regex_opt(item.pattern)!
		start, end := re.find(item.input)
		assert start == item.start, '${item.pattern} on ${item.input}'
		assert end == item.end, '${item.pattern} on ${item.input}'
	}
	mut re := regex.regex_opt('(a|b){2}z')!
	start, _ := re.find('az')
	assert start == -1
}

fn test_alternation_chain_matches_every_branch() {
	for pattern in ['a|b|c', 'c|b|a', '(a)|(b)|(c)'] {
		for input in ['a', 'b', 'c'] {
			mut re := regex.regex_opt(pattern)!
			start, end := re.find(input)
			assert start == 0, '${pattern} on ${input}'
			assert end == input.len
		}
	}
}

fn test_quantified_alternation_preserves_previous_matches() {
	for input in ['a', 'b', 'ab', 'baaba'] {
		mut re := regex.regex_opt('(a|b)+')!
		start, end := re.find(input)
		assert start == 0
		assert end == input.len
	}
	mut re := regex.regex_opt('d(a|b)+z')!
	start, end := re.find('xxdabbazyy')
	assert start == 2
	assert end == 8
	assert re.get_group_by_id('xxdabbazyy', 0) == 'a'
	not_found, _ := re.find('dcz')
	assert not_found == -1
}

fn test_alternation_keeps_documented_token_semantics() {
	mut re := regex.regex_opt('(cat|dog)')!
	for input in ['catog', 'cadog'] {
		start, end := re.find(input)
		assert start == 0
		assert end == input.len
	}
	for input in ['cat', 'dog'] {
		start, _ := re.find(input)
		assert start == -1
	}
	mut grouped := regex.regex_opt('((cat)|(dog))+')!
	start, end := grouped.find('dogcat')
	assert start == 0
	assert end == 6
}
