import regex

fn test_char_class_literal_hyphens() {
	for pattern in ['^[a-z-]+$', '^[a-z0-9-]+$', '^[a-z0-9_-]+$', '^[-a-z]+$', r'^[a-z\-]+$'] {
		mut re := regex.regex_opt(pattern) or { panic(err) }
		for text in ['-', 'a-c', 'abc', '---'] {
			assert re.matches_string(text), '${pattern}: ${text}'
		}
		for text in ['', 'A', 'a+c'] {
			assert !re.matches_string(text), '${pattern}: ${text}'
		}
	}
	mut re := regex.regex_opt('^[-]+$') or { panic(err) }
	assert re.matches_string('-')
	assert re.matches_string('---')
	assert !re.matches_string('a')
	mut negated := regex.regex_opt('^[^a-z-]+$') or { panic(err) }
	assert negated.matches_string('123_')
	assert !negated.matches_string('-')
	assert !negated.matches_string('abc')
}

fn test_char_class_minus_after_range() {
	patterns := ['^[a-z-0-9]+$', '^[0-9-a-z]+$', '^[a-b-c-d]+$', '^[a-z-A-Z]+$']
	matching := [
		['abc', '123', 'abc123', 'abc-123', '-'],
		['123', 'abc', '123-abc'],
		['abcd', 'abcd-', 'a-b-c-d'],
		['abc', 'xyz', 'abc-xyz'],
	]
	not_matching := [
		['', 'A', '_', '!'],
		['', 'A', '_'],
		['', 'e', 'A'],
		['', '123', '_'],
	]
	for i, pattern in patterns {
		re := regex.regex_opt(pattern) or { panic(err) }
		for text in matching[i] {
			assert re.matches_string(text), '${pattern}: ${text}'
		}
		for text in not_matching[i] {
			assert !re.matches_string(text), '${pattern}: ${text}'
		}
	}
}

fn test_or_with_multiple_character_group_tokens() {
	mut re := regex.regex_opt('^((cat)|(dog))$') or { panic(err) }
	assert re.matches_string('cat')
	assert re.matches_string('dog')
	assert !re.matches_string('catdog')
	assert !re.matches_string('cow')
}
