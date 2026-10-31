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

struct CastFoo {
	x int
}

struct CastBar {
	x int
}

type CastChoice = CastBar | CastFoo

fn choose_cast_selector(value CastChoice) int {
	return match true {
		(value as CastFoo).x > 0 { 1 }
		(value as CastBar).x > 0 { 2 }
		else { 0 }
	}
}

fn test_cast_target_distinguishes_selector_conditions() {
	assert choose_cast_selector(CastFoo{ x: 1 }) == 1
}

enum MatchChoice {
	first
	second
	third
}

fn choose_optional_cast(value ?MatchChoice) int {
	return match true {
		value == ?MatchChoice(.first) { 1 }
		value == ?MatchChoice(.second) { 2 }
		value == ?MatchChoice(.third) { 3 }
		else { 0 }
	}
}

fn test_optional_cast_operands_distinguish_match_conditions() {
	assert choose_optional_cast(.first) == 1
	assert choose_optional_cast(.second) == 2
	assert choose_optional_cast(.third) == 3
	assert choose_optional_cast(none) == 0
}
