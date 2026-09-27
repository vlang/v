struct Params {
	bytes  []int
	chars  []int
	fields []int
}

fn choose(p Params) int {
	return match true {
		p.bytes.len > 0 { 1 }
		p.chars.len > 0 { 2 }
		p.fields.len > 0 { 3 }
		else { 0 }
	}
}

fn test_distinct_conditions() {
	assert choose(Params{ bytes: [1] }) == 1
	assert choose(Params{ chars: [1] }) == 2
	assert choose(Params{ fields: [1] }) == 3
}

struct IndexedEntry {
	values []int
}

fn choose_indexed(lookup map[string]IndexedEntry, name string) int {
	return match true {
		lookup['name'].values.len > 0 { 1 }
		lookup[name].values.len > 0 { 2 }
		else { 0 }
	}
}

fn test_index_literal_and_identifier_conditions_are_distinct() {
	lookup := {
		'other': IndexedEntry{ values: [1] }
	}
	assert choose_indexed(lookup, 'other') == 2
}
