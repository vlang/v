import arrays

struct T {}

fn test_generic_array_membership_ignores_matching_concrete_type_name() {
	assert 'a' in arrays.flatten([['a']])
	assert arrays.flatten([['a']]).contains('a')
	assert arrays.flatten([['a']]).index('a') == 0
}
