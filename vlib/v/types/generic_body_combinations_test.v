module types

import v.flat

// The body of a generic function whose type parameters all have a constraint
// is checked once for each combination of the types of type_param_combinations
// (see check_generic_fn_body).

const combination_budget = 256
const combination_param_t = flat.source_type_id('T')
const combination_param_u = flat.source_type_id('U')

fn test_closing_numeric_type_choices_preserves_recursive_parameter_boundaries() {
	choices := {
		combination_param_t: flat.source_type_id('Box[U]')
		combination_param_u: flat.source_type_int
	}
	closed := closed_type_param_texts(choices)
	assert closed[combination_param_t] == flat.source_type_id('Box[int]')
	assert closed[combination_param_u] == flat.source_type_int
	assert choices[combination_param_t] == flat.source_type_id('Box[U]')
	recursive := {
		combination_param_t: flat.source_type_id('Box[T]')
	}
	assert closed_type_param_texts(recursive)[combination_param_t] == recursive[combination_param_t]
}

// numbered_texts gives each of `names` the types `<name>0`, `<name>1`, ... up
// to `count` of them.
fn numbered_texts(names []string, count int) map[u32][]u32 {
	mut texts := map[u32][]u32{}
	for name in names {
		texts[flat.source_type_id(name)] = []u32{len: count, init: flat.source_type_id('${name}${index}')}
	}
	return texts
}

// missing_pairs returns the pairs of types of two type parameters of `texts`
// that no combination of `combinations` gives them together.
fn missing_pairs(texts map[u32][]u32, combinations []map[u32]u32) []string {
	mut met := map[string]bool{}
	for combination in combinations {
		for first, a in combination {
			for second, b in combination {
				met['${flat.source_type_name(first)}=${flat.source_type_name(a)} ${flat.source_type_name(second)}=${flat.source_type_name(b)}'] = true
			}
		}
	}
	mut missing := []string{}
	for first, first_types in texts {
		for second, second_types in texts {
			if first == second {
				continue
			}
			for a in first_types {
				for b in second_types {
					pair := '${flat.source_type_name(first)}=${flat.source_type_name(a)} ${flat.source_type_name(second)}=${flat.source_type_name(b)}'
					if pair !in met {
						missing << pair
					}
				}
			}
		}
	}
	return missing
}

fn test_every_combination_of_the_types_is_checked_within_the_budget() {
	texts := {
		combination_param_t: [flat.source_type_int, flat.source_type_f64, flat.source_type_string]
		combination_param_u: [flat.source_type_bool, flat.source_type_rune]
	}
	combinations := type_param_combinations(texts, combination_budget)
	assert combinations.len == 6
	mut seen := map[string]bool{}
	for combination in combinations {
		seen['${combination[combination_param_t]} ${combination[combination_param_u]}'] = true
	}
	assert seen.len == 6
	// The first gives each type parameter its first type.
	assert combinations[0][combination_param_t] == flat.source_type_int
	assert combinations[0][combination_param_u] == flat.source_type_bool
}

fn test_a_single_type_parameter_is_checked_with_each_of_its_types() {
	texts := numbered_texts(['T'], combination_budget + 44)
	combinations := type_param_combinations(texts, combination_budget)
	assert combinations.len == combination_budget + 44
	mut seen := map[u32]bool{}
	for combination in combinations {
		seen[combination[combination_param_t]] = true
	}
	assert seen.len == combination_budget + 44
}

fn test_past_the_budget_every_two_type_parameters_meet_with_every_two_of_their_types() {
	// 13 * 13 * 13 = 2197 combinations: too many to check them all, but every
	// operation between two of them meets each two of their types.
	texts := numbered_texts(['A', 'B', 'C'], 13)
	combinations := type_param_combinations(texts, combination_budget)
	missing := missing_pairs(texts, combinations)
	assert missing.len == 0, missing#[..5].str()
	assert combinations.len <= combination_budget, combinations.len.str()
	for combination in combinations {
		assert combination.len == 3
	}
}

fn test_past_the_budget_for_pairs_each_type_parameter_takes_each_of_its_types() {
	// 20 * 20 = 400: every pair is every combination, past the budget.
	texts := numbered_texts(['T', 'U'], 20)
	combinations := type_param_combinations(texts, combination_budget)
	assert combinations.len == 1 + 19 + 19
	mut seen := map[string]bool{}
	for combination in combinations {
		for name, text in combination {
			seen['${name}=${text}'] = true
		}
	}
	assert seen.len == 40
}
