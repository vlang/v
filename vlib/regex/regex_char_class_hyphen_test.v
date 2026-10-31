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

fn test_char_class_minus_after_range_with_uppercase_and_negation() {
	positive := regex.regex_opt('^[a-z-A-Z]+$') or { panic(err) }
	for text in ['AZ', 'az', 'a-Z', '-'] {
		assert positive.matches_string(text)
	}
	negative := regex.regex_opt('^[^a-z-0-9]+$') or { panic(err) }
	for text in ['A', '_', '!'] {
		assert negative.matches_string(text)
	}
	for text in ['a', 'z', '-', '0', '9', 'A-a'] {
		assert !negative.matches_string(text)
	}
}

fn test_char_class_minus_after_range_preserves_escapes_and_class_boundaries() {
	escaped := regex.regex_opt(r'^[a-z\-0-9]+$') or { panic(err) }
	assert escaped.matches_string('abc-123')
	assert !escaped.matches_string('A')
	separate := regex.regex_opt('^[a-z][-0-9]+$') or { panic(err) }
	assert separate.matches_string('a-7')
	assert separate.matches_string('z09')
	assert !separate.matches_string('aA')
	assert !separate.matches_string('a')
}

fn test_char_class_minus_after_unicode_range() {
	re := regex.regex_opt('^[α-γ-0-9]+$') or { panic(err) }
	for text in ['αβγ', 'α-12', '-'] {
		assert re.matches_string(text)
	}
	for text in ['δ', 'A', '_'] {
		assert !re.matches_string(text)
	}
}

fn test_or_with_multiple_character_group_tokens() {
	mut re := regex.regex_opt('^((cat)|(dog))$') or { panic(err) }
	assert re.matches_string('cat')
	assert re.matches_string('dog')
	assert !re.matches_string('catdog')
	assert !re.matches_string('cow')
}
