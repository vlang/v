struct Rows {
mut:
	ints   []int
	names  []string
	counts map[string]int
	text   string
}

struct OptionalRows {
	values ?[]int
}

// The loops below are in non-generic functions: their bindings are typed from
// the reflected field, not from a generic instance.
fn test_for_in_over_reflected_fields_types_the_loop_bindings() {
	mut rows := Rows{
		ints:   [1, 2]
		names:  ['x', 'yz']
		counts: {
			'k': 5
		}
		text:   'ab'
	}
	mut total := 0
	mut seen := []string{}
	$for field in Rows.fields {
		$if field.typ is []int {
			for value in rows.$(field.name) {
				total += value
			}
			for mut value in rows.$(field.name) {
				value *= 10
			}
		} $else $if field.typ is []string {
			for i, value in rows.$(field.name) {
				seen << '${i}=${value}'
			}
		} $else $if field.typ is map[string]int {
			for key, value in rows.$(field.name) {
				seen << key
				total += value
			}
		} $else $if field.typ is string {
			for b in rows.$(field.name) {
				total += b
			}
		}
	}
	assert total == 1 + 2 + 5 + `a` + `b`
	assert seen == ['0=x', '1=yz', 'k']
	assert rows.ints == [10, 20]
}

fn test_for_in_over_reflected_option_field_with_fallback() {
	rows := OptionalRows{}
	mut total := 0
	$for field in OptionalRows.fields {
		for value in rows.$(field.name) or { [4, 5] } {
			total += value
		}
	}
	assert total == 9
}
