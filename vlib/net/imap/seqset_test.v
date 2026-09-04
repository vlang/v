module imap

fn test_single_numbers_collapse_into_ranges() {
	// The whole point of the set: four numbers go out as one range, because a
	// command line is not obliged to be long enough for the alternative.
	assert seq_set([u32(1), 2, 3, 4]).str() == '1:4'
	assert seq_set([u32(4), 3, 2, 1]).str() == '1:4'
	assert seq_set([u32(2), 84, 882]).str() == '2,84,882'
	assert seq_set([u32(1), 2, 3, 7, 8, 12]).str() == '1:3,7:8,12'
}

fn test_a_large_run_stays_short_on_the_wire() {
	mut all := []u32{}
	for i in u32(1) .. 50001 {
		all << i
	}
	assert seq_set(all).str() == '1:50000'
	assert seq_set(all).len() == 1
}

fn test_duplicates_and_overlaps_fold_together() {
	assert seq_set([u32(5), 5, 5]).str() == '5'
	mut s := seq_range(1, 10)
	s.add_range(5, 20)
	assert s.str() == '1:20'
	s.add_range(21, 21)
	assert s.str() == '1:21'
	// A gap of one is not a gap.
	mut t := seq_range(1, 3)
	t.add_range(4, 6)
	assert t.str() == '1:6'
	// A gap of two is.
	mut u := seq_range(1, 3)
	u.add_range(5, 6)
	assert u.str() == '1:3,5:6'
}

fn test_the_star_stands_for_the_last_message() {
	assert seq_all().str() == '1:*'
	assert seq_range(5, seq_star).str() == '5:*'
	assert seq_range(seq_star, seq_star).str() == '*'
	// `*` is the largest value there is, so a range given the other way round
	// still ends at it.
	assert seq_range(seq_star, 5).str() == '5:*'
}

fn test_an_open_range_swallows_what_follows_it() {
	mut s := seq_range(5, seq_star)
	s.add(9)
	s.add_range(100, 200)
	assert s.str() == '5:*'
	s.add(1)
	assert s.str() == '1,5:*'
}

fn test_ranges_are_ordered() {
	mut s := SeqSet{}
	s.add(100)
	s.add(3)
	s.add(50)
	assert s.str() == '3,50,100'
}

fn test_empty_set_renders_to_nothing() {
	s := SeqSet{}
	assert s.is_empty()
	assert s.str() == ''
	assert seq_set([]).is_empty()
}

fn test_parse_round_trip() {
	for text in ['1', '1:4', '2,84,882', '1:3,7:8,12', '5:*', '*', '1:*'] {
		assert parse_seq_set(text)!.str() == text, 'round trip failed for ${text}'
	}
	// A set written the long way comes back the short way, which is the same
	// set.
	assert parse_seq_set('1,2,3')!.str() == '1:3'
	assert parse_seq_set('4:2')!.str() == '2:4'
	assert parse_seq_set('')!.is_empty()
}

fn test_parse_rejects_what_is_not_a_set() {
	for bad in ['0', 'a', '1:', ':4', '1,,2', '4294967296', '1:2:3'] {
		parse_seq_set(bad) or { continue }
		assert false, '`${bad}` is not a sequence set but was accepted'
	}
}

fn test_contains() {
	s := parse_seq_set('1:3,7,10:*')!
	assert s.contains(1)
	assert s.contains(3)
	assert !s.contains(4)
	assert s.contains(7)
	assert !s.contains(8)
	assert s.contains(10)
	assert s.contains(999999)
}

fn test_numbers_expands_a_closed_set() {
	assert parse_seq_set('1:3,7')!.numbers()! == [u32(1), 2, 3, 7]
	// An open set has no end the client knows, so it cannot be expanded.
	parse_seq_set('5:*')!.numbers() or { return }
	assert false, 'expanding an open range must fail'
}
