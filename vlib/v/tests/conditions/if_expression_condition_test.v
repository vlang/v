fn conditional_search(text []u16, query []u16, case_sensitive bool) bool {
	for i, unit in query {
		if if case_sensitive { text[i] != unit } else { text[i] + 32 != unit } {
			return false
		}
	}
	return true
}

fn test_if_expression_condition_selects_and_evaluates_the_right_branch() {
	assert conditional_search([u16(65), 66], [u16(65), 66], true)
	assert !conditional_search([u16(65), 66], [u16(97), 98], true)
	assert conditional_search([u16(65), 66], [u16(97), 98], false)
	assert !conditional_search([u16(65), 66], [u16(65), 66], false)
}
