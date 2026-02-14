/*
regex_test.v

Copyright (c) 2026 Dario Deledda. All rights reserved.
Use of this source code is governed by an MIT license
that can be found in the LICENSE file.
*/
import regex.pcre

fn main() {
	println('Running pcre tests...\n')

	test_regex()
	test_complex_quantifiers()
	test_range_quantifiers()
	test_anchors()
	test_word_boundaries()
	test_flags()
	test_named_groups()
	test_non_capturing_groups()

	// New features tests
	test_find_all()
	test_find_from()
	test_replace()
	// test_stress_vm()

	println('\nAll tests passed!')
}

// --- New Feature Tests ---

fn test_find_all() {
	println('\n--- Testing find_all() ---')

	// Basic extraction
	tst_find_all(r'\d+', '123 abc 456', ['123', '456'])
	tst_find_all(r'\w+', 'hi there', ['hi', 'there'])

	// No matches
	tst_find_all(r'\d+', 'no numbers', [])

	// Pattern matching empty strings (e.g., boundaries)
	// Note: Behavior depends on engine implementation regarding empty matches.
	// Current VM advances index if match length is 0 to avoid infinite loop.
	// Pattern \b matches at 0 (start), 3 (after 123), 4 (before abc), 7 (after abc)
	// But find_all usually returns non-overlapping text. \b returns empty string.
	// tst_find_all(r'\b', '123 abc', ['', '', '', '']) // Commented out, specific implementation detail

	// Anchored find_all (should only match once if anchored at start)
	tst_find_all(r'^\w+', 'word word word', ['word'])

	// Overlapping logic check (find_all is typically non-overlapping)
	// "ana" in "banana". Indices: 1 ("ana"). Next search starts at 4 ("na").
	tst_find_all(r'ana', 'banana', ['ana'])
}

fn test_find_from() {
	println('\n--- Testing find_from() ---')

	text := 'test test test'

	// Start from 0 (finds first)
	tst_find_from(r'test', text, 0, 0, 'test')

	// Start from 1 (skips first, finds second)
	tst_find_from(r'test', text, 1, 5, 'test')

	// Start from 6 (finds third)
	tst_find_from(r'test', text, 6, 10, 'test')

	// Start from end (finds nothing)
	tst_find_from(r'test', text, 11, -1, 'none')

	// Out of bounds
	tst_find_from(r'test', text, 50, -1, 'none')

	// Start exactly at match position
	tst_find_from(r'test', text, 5, 5, 'test')
}

fn test_replace() {
	println('\n--- Testing replace() ---')

	// Simple replacement
	tst_replace(r'\d+', 'abc 123 def', 'NUM', 'abc NUM def')

	// Group substitution
	tst_replace(r'(\w+), (\w+)', 'Doe, John', '$2 $1', 'John Doe')

	// Multiple replacements?
	// The current replace() implementation in the provided code
	// usually replaces the FIRST occurrence (based on find()).
	// Let's verify:
	tst_replace(r'a', 'bananas', 'o', 'bonanas')

	tst_replace(r'(^[#.]+)|([#.]+$)', r'_#abc.#_ab#', '*', '_#abc.#_ab*')

	// Invalid group index (should ignore or remove)
	tst_replace(r'(\d+)', '123', 'Num: $9', 'Num: ')
}

/*
fn test_stress_vm() {
	println('\n--- Testing VM Stability (Stress Test) ---')
	// Recursive engines often crash on patterns like (a*)* or very long strings
	// if not carefully managed. The VM should handle this via heap stack.
	
	long_text := 'a'.repeat(200)
	tst_find(r'a+', long_text, long_text)
	
	println('  [Pass] Long string match')
	
	// Backtracking stress
	// Pattern: (a+)+b matching aaaaa....a (fails)
	// This forces extensive backtracking.
	short_text := 'a'.repeat(25)
	r := pcre.compile(r'(a+)+b') or { panic(err) }
	res := r.find(short_text)
	assert res == none
	println('  [Pass] Backtracking stress test')
}
*/

// --- Existing Tests ---

fn test_flags() {
	println('\n--- Testing Flags ((?i), (?m), (?s)) ---')

	// 1. Case Insensitive (?i)
	tst_find('(?i)cat', 'Cat', 'Cat')
	tst_find('(?i)CAT', 'cat', 'cat')
	tst_find('(?i)[a-z]+', 'UPPER', 'UPPER') // char class expansion
	tst_find('(?i)x', 'X', 'X')
	tst_find('(?i)x', 'y', 'none')

	// Mixed (flag applies to subsequent tokens)
	tst_find('c(?i)at', 'cAT', 'cAT')
	tst_find('c(?i)at', 'Cat', 'none') // first 'c' is case-sensitive

	// 2. Multiline (?m)
	// ^ matches start of line
	tst_find('(?m)^line2', 'line1\nline2', 'line2')
	tst_find('^line2', 'line1\nline2', 'none') // Default: matches only start of string

	// $ matches end of line
	tst_find('(?m)line1$', 'line1\nline2', 'line1')
	tst_find('line1$', 'line1\nline2', 'none') // Default: matches only end of string

	// 3. Dot-all / Singleline (?s)
	// . matches newline
	tst_find('(?s)a.b', 'a\nb', 'a\nb')
	tst_find('a.b', 'a\nb', 'none') // Default: . does not match \n

	// 4. Combined Flags (?im)
	tst_find('(?im)^line2', 'LINE1\nLINE2', 'LINE2')

	// --- Negative Tests (Flags) ---
	tst_find('(?i)cat', 'dog', 'none')
	tst_find('(?m)^line2', 'line1 line2', 'none') // Not at start of line
}

fn test_word_boundaries() {
	println('\n--- Testing Word Boundaries (\\b and \\B) ---')

	// 1. Word Boundary (\b)
	tst_find('\\bcat', 'cat', 'cat')
	tst_find('\\bcat', 'concat', 'none')
	tst_find('\\bcat', 'catapult', 'cat')

	tst_find('cat\\b', 'cat', 'cat')
	tst_find('cat\\b', 'concat', 'cat')
	tst_find('cat\\b', 'catapult', 'none')

	tst_find('\\bcat\\b', 'cat', 'cat')
	tst_find('\\bcat\\b', 'a cat is here', 'cat')
	tst_find('\\bcat\\b', 'concat', 'none')
	tst_find('\\bcat\\b', 'catapult', 'none')

	tst_find('\\btest\\b', 'test.', 'test')
	tst_find('\\btest\\b', '(test)', 'test')

	// 2. Non-Word Boundary (\B)
	tst_find('a\\B', 'ab', 'a')
	tst_find('a\\B', 'a.', 'none')
	tst_find('\\Bcat', 'concat', 'cat')
	tst_find('\\Bcat', 'cat', 'none')
	tst_find('cat\\B', 'catapult', 'cat')

	// --- Negative Tests (Word Boundaries) ---
	tst_find('\\b\\w+\\b', '... ...', 'none')
	tst_find('\\B', 'a', 'none')
}

fn test_anchors() {
	println('\n--- Testing Anchors (^ and $) ---')

	// 1. Start of String (^)
	tst_find('^abc', 'abc', 'abc')
	tst_find('^abc', 'abcdef', 'abc')
	tst_find('^abc', 'abc abc', 'abc')
	tst_find('^\\d+', '123 text', '123')

	// 2. End of String ($)
	tst_find('xyz$', 'xyz', 'xyz')
	tst_find('xyz$', 'abcxyz', 'xyz')
	tst_find('\\d+$', 'text 123', '123')

	// 3. Both Anchors (^...$)
	tst_find('^hello$', 'hello', 'hello')

	// 4. Zero-width matches
	tst_find('^', 'abc', '')
	tst_find('$', 'abc', '')

	// 5. Anchors with Alternation
	tst_find('^a|b$', 'apple', 'a')
	tst_find('^a|b$', 'blob', 'b')

	// 6. Anchors with Groups
	tst_find('^(abc)+$', 'abcabc', 'abcabc')

	// --- Negative Tests (Anchors) ---
	tst_find('^abc', 'xyzabc', 'none')
	tst_find('^\\d+', 'text 123', 'none')
	tst_find('xyz$', 'xyzabc', 'none')
	tst_find('\\d+$', '123 text', 'none')
	tst_find('^hello$', 'hello world', 'none')
	tst_find('^hello$', 'say hello', 'none')
	tst_find('^a|b$', 'cba', 'none')
	tst_find('^(abc)+$', 'abcabcx', 'none')
	tst_find('^$', 'a', 'none')
}

fn test_regex() {
	println('\n--- Testing Basic Features ---')
	tst_find('a?b', 'ab', 'ab')
	tst_find('a?b', 'b', 'b')
	tst_find('a+b', 'aaab', 'aaab')
	tst_find('a*b', 'b', 'b')
	tst_find('\\d+', '123 abc', '123')

	println('\n--- Testing Character Classes ---')
	tst_find('\\w+', 'word1_ and', 'word1_')
	tst_find('\\W+', ' and', ' ')
	tst_find('\\s+', '          start', '          ')
	tst_find('\\d{3}-\\d{4}', 'call 555-1234 now', '555-1234')
	tst_find('\\D+', 'call 555', 'call ')
	tst_find('\\a+', 'lowercase', 'lowercase')
	tst_find('\\A+', 'UPPER', 'UPPER')

	println('\n--- Testing Alternation (|) ---')
	tst_find('cat|dog', 'the dog says meow', 'dog')
	tst_find('a(b|c)d', 'acd', 'acd')
	tst_find('apple|apply', 'I want to apply', 'apply')

	println('\n--- Testing Custom Character Classes ([...]) ---')
	tst_find('[aeiou]', 'hello world', 'e')
	tst_find('gr[ae]y', 'the color grey', 'grey')
	tst_find('[^aeiou]+', 'rhythm', 'rhythm')
	tst_find('[a-z]+', 'lowercase123', 'lowercase')
	tst_find('[a-zA-Z0-9_]+', 'word_1_with_everything', 'word_1_with_everything')

	println('\n--- Testing Unicode ---')
	tst_find('日本語', 'Text containing 日本語.', '日本語')
	tst_find('h.llo', 'héllo wørld', 'héllo')
	tst_find('(é)+', 'cafééé', 'ééé')
	tst_find('😀+', 'Happy 😀😀 day', '😀😀')

	println('\n--- Testing fullmatch() ---')
	tst_fullmatch(r'\d+', '12345', '12345')
	tst_fullmatch('(?s).*', 'Any content including 😀', 'Any content including 😀')

	// --- Negative Tests (Basic) ---
	tst_find('abc', 'ab', 'none')
	tst_find('abc', 'acb', 'none')
	tst_find('a+b', 'b', 'none')
	tst_find('\\d+', 'abc', 'none')
	tst_find('\\D+', '123', 'none')
	tst_find('\\w+', '@#$', 'none')
	tst_find('\\s+', 'Text', 'none')
	tst_find('\\a+', 'UPPERCASE', 'none')
	tst_find('\\A+', 'lowercase', 'none')
	tst_find('cat|dog', 'bird', 'none')
	tst_find('[0-9]', 'a', 'none')
	tst_find('[^0-9]', '1', 'none')
	tst_fullmatch(r'\d+', '12345abc', 'none')
	tst_fullmatch(r'\d+', 'abc12345', 'none')

	println('\n--- Testing Compilation Errors ---')
	tst_compile_error('a++')
	tst_compile_error('[a-z')
	tst_compile_error('a|')
}

fn test_complex_quantifiers() {
	println('\n--- Testing Complex Quantifiers (+, *, ?) ---')

	tst_find('a+', 'aaaaa', 'aaaaa')
	tst_find('a+b', 'aaaaab', 'aaaaab')

	tst_find('x*y', 'y', 'y')
	tst_find('x*y', 'xy', 'xy')
	tst_find('x*y', 'xxxy', 'xxxy')

	tst_find('colou?r', 'color', 'color')
	tst_find('colou?r', 'colour', 'colour')
	tst_find('x?y', 'xy', 'xy')
	tst_find('x?y', 'y', 'y')

	tst_find('(ab)+', 'ababab', 'ababab')
	tst_find('(ha)+', 'hahaha!', 'hahaha')

	tst_find('(cat|dog)+', 'catdogcat', 'catdogcat')
	tst_find('(a|b)+', 'abaabbba', 'abaabbba')

	tst_find('[0-9]+', 'Order 12345', '12345')
	tst_find('[a-z]*', '123', '')

	// --- Negative Tests (Complex Quantifiers) ---
	tst_find('a+', '', 'none')
	tst_find('a+', 'b', 'none')
	tst_find('a+b', 'aaac', 'none')
	tst_find('x?y', 'x', 'none')
	tst_find('(ab)+', 'ac', 'none')
	tst_find('[0-9]+', 'abc', 'none')
}

fn test_range_quantifiers() {
	println('\n--- Testing Range Quantifiers {m,n} ---')

	tst_find('a{3}', 'aaa', 'aaa')
	tst_find('a{3}', 'aaaa', 'aaa')

	tst_find('a{2,}', 'aa', 'aa')
	tst_find('a{2,}', 'aaaaa', 'aaaaa')

	tst_find('a{,3}', 'aaaa', 'aaa')
	tst_find('a{,3}', 'aa', 'aa')
	tst_find('a{,3}', '', '')

	tst_find('a{2,4}', 'aa', 'aa')
	tst_find('a{2,4}', 'aaa', 'aaa')
	tst_find('a{2,4}', 'aaaa', 'aaaa')
	tst_find('a{2,4}', 'aaaaa', 'aaaa')

	tst_find(r'\d{2,4}-\w{2}', '123-ab', '123-ab')
	tst_find(r'\d{2,4}-\w{2}', '12345-ab', '2345-ab')

	// --- Negative Tests (Range Quantifiers) ---
	tst_find('a{3}', 'aa', 'none')
	tst_find('a{2,}', 'a', 'none')
	tst_find('a{2,4}', 'a', 'none')
	tst_find(r'\d{2,4}-\w{2}', '1-ab', 'none')
	tst_find(r'\d{2,4}-\w{2}', '123-a', 'none')
}

fn test_named_groups() {
	println('\n--- Testing Named Groups ---')

	pattern := '(?P<year>\\d{4})-(?P<month>\\d{2})'
	text := 'Date: 2025-01'
	r := pcre.compile(pattern) or { panic(err) }
	m := r.find(text) or { panic('Match not found') }

	assert m.groups[0] == '2025'
	assert m.groups[1] == '01'

	assert r.group_by_name(m, 'year') == '2025'
	assert r.group_by_name(m, 'month') == '01'
	assert r.group_by_name(m, 'missing') == ''

	nested_pat := '(?P<entry>key: (?P<val>\\d+))'
	nested_txt := 'List [ key: 99 ]'
	r_nested := pcre.compile(nested_pat) or { panic(err) }
	m_nested := r_nested.find(nested_txt) or { panic('Match not found') }

	println('Nested: entry="${r_nested.group_by_name(m_nested, 'entry')}", val="${r_nested.group_by_name(m_nested,
		'val')}"')
	assert r_nested.group_by_name(m_nested, 'entry') == 'key: 99'
	assert r_nested.group_by_name(m_nested, 'val') == '99'

	pattern_mixed := '(?P<key>\\w+): (\\d+)'
	text_mixed := 'Price: 100'
	r_mixed := pcre.compile(pattern_mixed) or { panic(err) }
	m_mixed := r_mixed.find(text_mixed) or { panic('Match not found') }

	assert m_mixed.groups[0] == 'Price'
	assert m_mixed.groups[1] == '100'
	assert r_mixed.group_by_name(m_mixed, 'key') == 'Price'

	p_seq := '(?P<a>a)(?P<b>b)(?P<c>c)'
	t_seq := 'abc'
	r_seq := pcre.compile(p_seq) or { panic(err) }
	m_seq := r_seq.find(t_seq) or { panic('Match not found') }
	assert r_seq.group_by_name(m_seq, 'a') == 'a'
	assert r_seq.group_by_name(m_seq, 'b') == 'b'
	assert r_seq.group_by_name(m_seq, 'c') == 'c'

	// --- Negative Tests (Named Groups) ---
	if _ := r.find('Date: 99-01') {
		assert false, 'Should not match'
	} else {
		println('Found: none (Expected: none)')
	}
	tst_find('(?P<id>\\d+)', 'abc', 'none')
}

fn test_non_capturing_groups() {
	println('\n--- Testing Non-Capturing Groups ---')

	tst_find_with_groups('(?:a|b)c', 'ac', 'ac', [])

	tst_find_with_groups('(a)(?:b)(c)', 'abc', 'abc', ['a', 'c'])

	tst_find_with_groups('(a(?:b)c)', 'abc', 'abc', ['abc'])

	tst_find_with_groups('(?:header): (\\d+)', 'header: 123', 'header: 123', ['123'])

	// --- Negative Tests (Non-Capturing Groups) ---
	tst_find('(?:a|b)c', 'dc', 'none')
	tst_find('(?:a)b', 'c', 'none')
}

// --- Helper Functions ---

fn tst_find(pattern string, text string, expected string) {
	print('[find] Pattern: "${pattern}", Text: "${text}" -> ')
	r := pcre.compile(pattern) or {
		println('Compile error: ${err}')
		assert false, 'Unexpected compile error: ${err}'
		return
	}
	match_res := r.find(text)
	check_result(match_res, expected)
}

fn tst_find_all(pattern string, text string, expected []string) {
	print('[find_all] Pattern: "${pattern}", Text: "${text}" -> ')
	r := pcre.compile(pattern) or { panic(err) }
	matches := r.find_all(text)

	mut res_strs := []string{}
	for m in matches {
		res_strs << m.text
	}

	println('Found: ${res_strs}')
	assert res_strs == expected
}

fn tst_find_from(pattern string, text string, start int, expected_pos int, expected_text string) {
	print('[find_from] Pattern: "${pattern}", Start: ${start} -> ')
	r := pcre.compile(pattern) or { panic(err) }
	match_res := r.find_from(text, start)

	if match_res != none {
		println('Found: "${match_res.text}" at ${match_res.start}')
		assert match_res.text == expected_text
		assert match_res.start == expected_pos
	} else {
		println('Found: none')
		assert expected_text == 'none'
	}
}

fn tst_replace(pattern string, text string, repl string, expected string) {
	print('[replace] Pattern: "${pattern}", Repl: "${repl}" -> ')
	r := pcre.compile(pattern) or { panic(err) }
	res := r.replace(text, repl)
	println('Result: "${res}"')
	assert res == expected
}

fn tst_fullmatch(pattern string, text string, expected string) {
	print('[fullmatch] Pattern: "${pattern}", Text: "${text}" -> ')
	r := pcre.compile(pattern) or {
		println('Compile error: ${err}')
		assert false, 'Unexpected compile error: ${err}'
		return
	}
	match_res := r.fullmatch(text)
	check_result(match_res, expected)
}

fn check_result(match_res ?pcre.Match, expected string) {
	if match_res != none {
		println('Found: "${match_res.text}" (Expected: "${expected}")')
		assert match_res.text == expected
	} else {
		println('Found: none (Expected: "${expected}")')
		assert expected == 'none'
	}
}

fn tst_find_with_groups(pattern string, text string, expected_match string, expected_groups []string) {
	print('[find+groups] Pattern: "${pattern}", Text: "${text}" -> ')
	r := pcre.compile(pattern) or {
		println('Compile error: ${err}')
		assert false, 'Unexpected compile error: ${err}'
		return
	}
	match_res := r.find(text)
	if match_res != none {
		println('Found: "${match_res.text}", Groups: ${match_res.groups}')
		assert match_res.text == expected_match
		assert match_res.groups == expected_groups
	} else {
		println('Found: none')
		assert false // Should have found a match_res
	}
}

fn tst_compile_error(pattern string) {
	print('[compile_error] Pattern: "${pattern}" -> ')
	_ := pcre.compile(pattern) or {
		println('Caught expected error: ${err}')
		return
	}
	println('Error: Did not get a compilation error!')
	assert false
}

fn test_non_greedy_quantifiers() {
	println('\n--- Testing Non-Greedy Quantifiers (*?, +?, ??, {m,n}?) ---')

	// 1. Lazy Star (*?)
	// Should stop at the first closing '>' (minimal match)
	tst_find(r'<.*?>', '<div>content</div>', '<div>')
	// Contrast with greedy (default) which consumes until the last '>'
	tst_find(r'<.*>', '<div>content</div>', '<div>content</div>')

	// 2. Lazy Plus (+?)
	// Should match minimal characters (1 'a') to satisfy the constraint
	tst_find(r'a+?', 'aaaaa', 'a')
	// Forced expansion: Must match all 'a's to finally match 'b' (backtracking test)
	tst_find(r'a+?b', 'aaab', 'aaab')

	// 3. Lazy Question Mark (??)
	// Should match empty string (prefers 0 occurrences over 1)
	tst_find(r'a??', 'a', '')
	// Contextual: 'u' is lazy (prefers skip), matches 'color' immediately
	tst_find(r'colou??r', 'color', 'color')
	// Contextual: 'u' is lazy, tries skip, fails to match 'r', backtracks to match 'u'
	tst_find(r'colou??r', 'colour', 'colour')

	// 4. Lazy Range ({m,n}?)
	// Should match minimum required (2 digits)
	tst_find(r'\d{2,5}?', '123456789', '12')
	// Contrast with greedy which matches maximum (5 digits)
	tst_find(r'\d{2,5}', '123456789', '12345')

	// 5. Complex/Real-world Case (User report)
	// Escaped characters + lazy capture group
	// Should match only '$t(common.hello)', not the span to the second ')'
	tst_find(r'\$t\((.*?)\)', r'$t(common.hello) dear $t(common.name)', r'$t(common.hello)')

	// --- Negative / Edge Cases ---

	// Lazy quantifier with no termination in string should match nothing/min if possible,
	// but since it's "find", it grabs the first valid match.
	tst_find(r'x.*?y', 'x123y456y', 'x123y') // Stops at first y

	// Anchor interaction: ^.*?b
	// Matches from start, .*? expands lazily until it hits 'b'
	tst_find(r'^.*?b', '123b', '123b')

	// Ensure lazy doesn't cause failure when a greedy match would succeed (correct backtracking)
	// Pattern wants to match "a" lazily, but must consume "a" to satisfy the final "a"
	tst_find(r'a?a', 'a', 'a')
	tst_find(r'a??a', 'a', 'a')
}

fn test_compatibility_layer() {
	// Test new_regex (alias for compile)
	// Passing '0' as the second argument to simulate the ignored C-flag argument
	pattern := r'(\w+)\s+(\d+)'
	re := pcre.new_regex(pattern, 0) or {
		assert false, 'new_regex failed to compile: ${err}'
		return
	}

	text := 'item 42 ignored item 99'

	// Test match_str (alias for find_from)
	// We start searching from index 0. The third argument '0' is the ignored option flag.
	// This should match "item 42"
	m1 := re.match_str(text, 0, 0) or {
		assert false, 'match_str failed to find match'
		return
	}

	// Test get()
	// Index 0 should be the full text of the match
	full_match := m1.get(0) or { '' }
	assert full_match == 'item 42'

	// Index 1 should be the first capture group (\w+)
	group_1 := m1.get(1) or { '' }
	assert group_1 == 'item'

	// Index 2 should be the second capture group (\d+)
	group_2 := m1.get(2) or { '' }
	assert group_2 == '42'

	// Index 3 should be none (out of bounds)
	if _ := m1.get(3) {
		assert false, 'get(3) should return none for 2 groups'
	}

	// Test get_all()
	// Should return ['item 42', 'item', '42']
	all_captures := m1.get_all()
	assert all_captures.len == 3
	assert all_captures[0] == 'item 42'
	assert all_captures[1] == 'item'
	assert all_captures[2] == '42'

	// Test match_str with a specific start index
	// Start searching after "item 42" (length is 7)
	// This should match "item 99"
	m2 := re.match_str(text, 7, 0) or {
		assert false, 'match_str failed to find second match from offset'
		return
	}

	assert m2.get(0) or { '' } == 'item 99'
	assert m2.get(2) or { '' } == '99'

	// Test match_str failure case
	// Start searching at the very end of string
	no_match := re.match_str(text, text.len, 0)
	if _ := no_match {
		assert false, 'match_str should return none when no match is found'
	}
}
