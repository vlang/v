module types

// closest_identifier_span picks the whole-word occurrence of `name` nearest to
// `anchor`; a left match wins a tie. These pin the search boundaries after the
// right-first bounded scan replaced the unbounded walk to the file start.

fn span_start(source string, name string, anchor int) ?int {
	pos := closest_identifier_span(source, name, anchor, 1)?
	assert pos.end == pos.offset + name.len
	return pos.offset
}

fn test_match_exactly_at_the_anchor_needs_no_search() {
	source := 'x := y\nfoo := 1\nbar := foo'
	assert span_start(source, 'foo', 7)? == 7
}

fn test_nearest_match_on_each_side_and_left_wins_ties() {
	//         0123456789012345678901
	source := 'foo a foo b foo c foo'
	// Anchor in the middle of ` b `: left `foo` at 6 is 3 away, right at 12 is 3 away.
	assert span_start(source, 'foo', 9)? == 6
	// One step to the right, the right match is nearer.
	assert span_start(source, 'foo', 10)? == 12
	// One step to the left, the left match is nearer.
	assert span_start(source, 'foo', 8)? == 6
}

fn test_embedded_substrings_are_not_matches() {
	//         012345678901234567890123
	source := 'foobar barfoo _foo foo_ foo'
	// Every earlier occurrence is part of a longer identifier; only the last is a word.
	assert span_start(source, 'foo', 0)? == 24
	assert span_start(source, 'foo', 12)? == 24
	// A substring on the right must not stop the right search short of a real
	// match, and a substring on the left must not be reported as the left match.
	assert span_start('foo foobar foo', 'foo', 6)? == 11
}

fn test_match_only_on_one_side_or_nowhere() {
	assert span_start('foo bar baz', 'foo', 10)? == 0
	assert span_start('bar baz foo', 'foo', 0)? == 8
	assert closest_identifier_span('bar baz', 'foo', 3, 1) == none
	assert closest_identifier_span('foobar', 'foo', 0, 1) == none
	assert closest_identifier_span('foo', '', 0, 1) == none
	assert closest_identifier_span('fo', 'foo', 0, 1) == none
}

fn test_anchor_outside_the_source_is_clamped() {
	source := 'foo bar'
	assert span_start(source, 'bar', 100)? == 4
	assert span_start(source, 'foo', -5)? == 0
}
