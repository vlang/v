// Regression test for https://github.com/vlang/v/issues/27867
// or-unwrapping an option map (struct field, variable, index, parenthesized)
// into an immutable variable used to fail with
// `cannot copy map: call move or clone method (or use a reference)`.
// The mutable form (`mut x := m or { ... }`) still errors on purpose, see
// vlib/v/checker/tests/option_map_or_unwrap_mut_alias_err.vv.
struct Translation {
	title string
}

struct Category {
	title        string
	translations ?map[string]Translation
}

fn test_option_map_field_or_unwrap() {
	c := Category{
		translations: {
			'en': Translation{
				title: 'Hello'
			}
		}
	}
	translations := c.translations or { panic('expected') }
	assert translations['en'].title == 'Hello'
}

fn test_option_map_field_or_unwrap_none() {
	c := Category{}
	translations := c.translations or {
		map[string]Translation{}
	}

	assert translations.len == 0
}

fn get_option_map() ?map[string]int {
	return {
		'a': 1
	}
}

fn test_option_map_var_or_unwrap() {
	x := get_option_map()
	y := x or { panic('expected') }
	assert y['a'] == 1
}

fn test_option_map_index_or_unwrap() {
	inner_map := {
		'b': 2
	}
	m := {
		'a': inner_map
	}
	inner := m['a'] or { panic('expected') }
	assert inner['b'] == 2
}

fn test_option_map_field_paren_or_unwrap() {
	c := Category{
		translations: {
			'en': Translation{
				title: 'Hello'
			}
		}
	}
	translations := (c.translations or { panic('expected') })
	assert translations['en'].title == 'Hello'
}
