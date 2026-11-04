import os

struct MatchItem {
	n int
}

struct MatchName {
	s string
}

type MatchValue = MatchItem | MatchName
type MatchIS = int | string
type MatchNum = int | i64 | bool
type MatchFloat = f64 | string
type MatchPromoted = int | f64 | string

enum MatchColor {
	red
	blue
}

type MatchColorValue = MatchColor | string
type MatchText = string
type MatchTextValue = MatchText | string

struct MatchBox {
	v MatchValue
}

fn test_match_sum_values_at_storage_boundaries() {
	for k in 1 .. 3 {
		mut m := map[string]MatchValue{}
		m['key'] = match k {
			1 { MatchValue(MatchItem{ n: k }) }
			else { MatchName{ s: 'x' } }
		}
		v := m['key'] or { panic('missing key') }
		assert (v is MatchItem) == (k == 1)
		mut a := []MatchValue{}
		a << match k {
			1 { MatchValue(MatchItem{ n: k }) }
			else { MatchName{ s: 'x' } }
		}
		assert (a[0] is MatchItem) == (k == 1)
		box := MatchBox{
			v: match k {
				1 { MatchValue(MatchItem{ n: k }) }
				else { MatchName{ s: 'x' } }
			}
		}
		assert (box.v is MatchItem) == (k == 1)
		mut scalars := map[int]MatchIS{}
		scalars[k] = match k {
			1 { 5 }
			else { 'x' }
		}
		scalar := scalars[k] or { panic('missing scalar') }
		assert (scalar is int) == (k == 1)
		mut numbers := map[int]MatchNum{}
		numbers[k] = match k {
			1 { 5 }
			else { true }
		}
		number := numbers[k] or { panic('missing number') }
		assert (number is bool) == (k != 1)
	}
}

fn test_match_sum_storage_keeps_numeric_enum_and_alias_types() {
	for k in 1 .. 3 {
		mut numbers := map[int]MatchFloat{}
		numbers[k] = match k {
			1 { 5 }
			else { 2.5 }
		}
		value := numbers[k] or { panic('missing number') }
		assert value is f64
		assert (value as f64) == if k == 1 { 5.0 } else { 2.5 }
		mut colors := map[int]MatchColorValue{}
		colors[k] = match k {
			1 { MatchColor.red }
			else { .blue }
		}
		color := colors[k] or { panic('missing color') }
		assert color is MatchColor
		mut texts := map[int]MatchTextValue{}
		texts[k] = match k {
			1 { MatchText('a') }
			else { 'b' }
		}
		text := texts[k] or { panic('missing text') }
		match text {
			MatchText {
				assert k == 1
			}
			string {
				assert k != 1
			}
		}
		mut promoted := map[int]MatchPromoted{}
		promoted[k] = match k {
			1 { 5 }
			else { 2.5 }
		}
		promoted_value := promoted[k] or { panic('missing promoted number') }
		assert promoted_value is f64
		promoted[k] = match k {
			1 {
				n := 5
				n
			}
			else {
				n := 2.5
				n
			}
		}
		named_value := promoted[k] or { panic('missing named number') }
		assert named_value is f64
	}
}

fn test_match_sum_storage_uses_the_current_branch_binding() {
	mut m := map[int]MatchValue{}
	for k in 1 .. 3 {
		if k == 1 {
			value := MatchItem{ n: 8 }
			assert value.n == 8
		}
		m[k] = match k {
			1 {
				value := MatchName{ s: 'branch' }
				value
			}
			else {
				value := MatchItem{ n: 2 }
				value
			}
		}
		value := m[k] or { panic('missing value') }
		assert (value is MatchName) == (k == 1)
	}
}

struct MatchDefault {
	v MatchValue = match default_key() {
		1 { MatchValue(MatchItem{ n: 1 }) }
		else { MatchName{ s: 'default' } }
	}
}

struct IfDefault {
	v MatchValue = if default_key() == 1 {
		MatchValue(MatchItem{ n: 1 })
	} else {
		MatchName{ s: 'default' }
	}
}

fn default_key() int {
	os.setenv('V_MATCH_DEFAULT_CALLS', (os.getenv('V_MATCH_DEFAULT_CALLS').int() + 1).str(), true)
	return 2
}

fn test_sum_field_defaults_run_for_each_array_element() {
	os.setenv('V_MATCH_DEFAULT_CALLS', '0', true)
	defer { os.unsetenv('V_MATCH_DEFAULT_CALLS') }
	empty := []MatchDefault{len: 0}
	assert empty.len == 0
	assert os.getenv('V_MATCH_DEFAULT_CALLS') == '0'
	matches := []MatchDefault{len: 3}
	assert os.getenv('V_MATCH_DEFAULT_CALLS') == '3'
	conditions := []IfDefault{len: 3}
	assert os.getenv('V_MATCH_DEFAULT_CALLS') == '6'
	for v in matches {
		assert v.v is MatchName
	}
	for v in conditions {
		assert v.v is MatchName
	}
}

fn match_item() MatchItem { return MatchItem{ n: 9 } }

fn test_match_sum_storage_with_calls_and_parentheses() {
	mut values := map[int]MatchValue{}
	for k in 1 .. 3 {
		values[k] = match k {
			1 { match_item() }
			else { (MatchName{ s: 'parenthesized' }) }
		}
		value := values[k] or { panic('missing value') }
		assert (value is MatchItem) == (k == 1)
	}
}

struct MatchNumberField {
	value f64
}

struct MatchStringField {
	value string
}

fn test_match_sum_storage_uses_the_current_branch_selector() {
	mut values := map[int]MatchFloat{}
	for k in 1 .. 3 {
		if k == 1 {
			item := MatchNumberField{ value: 3.0 }
			assert item.value == 3.0
		}
		values[k] = match k {
			1 {
				item := MatchStringField{ value: 'current' }
				item.value
			}
			else {
				item := MatchNumberField{ value: 4.0 }
				item.value
			}
		}
		value := values[k] or { panic('missing selector value') }
		assert (value is string) == (k == 1)
	}
}

enum MatchOtherColor {
	red
	blue
}

type MatchTwoColors = MatchOtherColor | MatchColor | string

fn test_match_sum_storage_infers_the_qualified_enum_for_shorthand() {
	mut values := map[int]MatchTwoColors{}
	for k in 1 .. 3 {
		values[k] = match k {
			1 { MatchColor.red }
			else { .blue }
		}
		value := values[k] or { panic('missing enum') }
		match value {
			MatchColor {
				assert value == if k == 1 { MatchColor.red } else { MatchColor.blue }
			}
			else {
				assert false
			}
		}
	}
}
