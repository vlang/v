// A string literal whose text is the name of a type alias is a string, also as the element of
// an array literal that is compared, or that infers a generic parameter.
type Speed = i32

fn first[T](a []T) T {
	return a[0]
}

fn test_comparison_with_an_array_literal_of_such_strings() {
	names := ['Speed', 'Temp']
	assert names == ['Speed', 'Temp']
	assert ['Speed'] != ['Temp']
}

fn test_generic_inferred_from_an_array_literal_of_such_strings() {
	assert first(['Speed']) == 'Speed'
	assert typeof(first(['Speed'])).name == 'string'
	assert first([Speed(7)]) == Speed(7)
}

fn test_alias_text_in_interpolated_and_parenthesized_literals() {
	name := 'Speed'
	assert first(['${name}']) == 'Speed'
	assert typeof(first(['${name}'])).name == 'string'
	assert first([('Speed')]) == 'Speed'
	mut names := ['Speed']
	names << 'Temp'
	assert names == ['Speed', 'Temp']
}
