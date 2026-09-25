module main

import nostr
import withstr

// A `str` method declared for `withstr.Any`, `[]withstr.Any` or
// `map[string]withstr.Any` must not be used to stringify the same-named types
// from `nostr` (`json2.Any` vs `toml.Any`).
fn test_sum_type_str_does_not_use_same_named_type_methods() {
	arr := nostr.Any([nostr.Any(1), nostr.Any('s')])
	for s in [arr.string(), arr.str(), '${arr}'] {
		assert !s.contains('withstr'), s
		assert s.starts_with('Any(['), s
	}
	m := nostr.Any({
		'k': nostr.Any(2)
	})
	for s in [m.string(), m.str(), '${m}'] {
		assert !s.contains('withstr'), s
		assert s.starts_with('Any({'), s
	}
	assert nostr.Any(3).string() == 'Any(3)'
	assert nostr.Any('x').string() == 'x'
	assert withstr.Any(1).str() == 'withstr-any'
}

fn test_array_and_map_str_do_not_use_same_named_type_methods() {
	arr := [nostr.Any(1), nostr.Any('s')]
	for s in [arr.str(), arr.text(), '${arr}'] {
		assert !s.contains('withstr'), s
		assert s.contains('Any(1)'), s
		assert s.contains("Any('s')"), s
	}
	m := {
		'k': nostr.Any(2)
	}
	for s in [m.str(), '${m}'] {
		assert !s.contains('withstr'), s
		assert s.contains('Any(2)'), s
	}
	assert [withstr.Any(1)].str() == 'withstr-array'
	assert {
		'k': withstr.Any(1)
	}.str() == 'withstr-map'
}

fn test_struct_and_enum_str_do_not_use_same_named_type_methods() {
	p := nostr.Point{
		x: 7
	}
	for s in [p.str(), p.text(), '${p}'] {
		assert !s.contains('withstr'), s
		assert s.contains('x: 7'), s
	}
	c := nostr.Color.red
	for s in [c.str(), c.text(), '${c}'] {
		assert s == 'red', s
	}
	assert withstr.Point{}.str() == 'withstr-point'
	assert withstr.Color.red.str() == 'withstr-color'
}
