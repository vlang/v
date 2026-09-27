module types

// The body of a generic function whose type parameters all have a constraint
// is checked once for each combination of the types of type_param_combinations
// (see check_generic_fn_body).

const combination_budget = 256

// numbered_texts gives each of `names` the types `<name>0`, `<name>1`, ... up
// to `count` of them.
fn numbered_texts(names []string, count int) map[string][]string {
	mut texts := map[string][]string{}
	for name in names {
		texts[name] = []string{len: count, init: '${name}${index}'}
	}
	return texts
}

// missing_pairs returns the pairs of types of two type parameters of `texts`
// that no combination of `combinations` gives them together.
fn missing_pairs(texts map[string][]string, combinations []map[string]string) []string {
	mut met := map[string]bool{}
	for combination in combinations {
		for first, a in combination {
			for second, b in combination {
				met['${first}=${a} ${second}=${b}'] = true
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
					pair := '${first}=${a} ${second}=${b}'
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
		'T': ['int', 'f64', 'string']
		'U': ['bool', 'rune']
	}
	combinations := type_param_combinations(texts, combination_budget)
	assert combinations.len == 6
	mut seen := map[string]bool{}
	for combination in combinations {
		seen['${combination['T']} ${combination['U']}'] = true
	}
	assert seen.len == 6
	// The first gives each type parameter its first type.
	assert combinations[0]['T'] == 'int'
	assert combinations[0]['U'] == 'bool'
}

fn test_a_single_type_parameter_is_checked_with_each_of_its_types() {
	texts := numbered_texts(['T'], combination_budget + 44)
	combinations := type_param_combinations(texts, combination_budget)
	assert combinations.len == combination_budget + 44
	mut seen := map[string]bool{}
	for combination in combinations {
		seen[combination['T']] = true
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
