struct ReflectedVariadicFields {
	number int
	label  string
	items  []int
	nested [][]int
}

fn reflected_collect[T](values ...T) []T {
	return values
}

fn collect_reflected_fields[T](object T) []string {
	mut output := []string{}
	$for field in T.fields {
		values := reflected_collect(object.$(field.name))
		assert values.len == 1
		assert values == [object.$(field.name)]
		output << values.str()
	}
	return output
}

fn test_reflected_fields_infer_scalar_and_array_variadic_elements() {
	object := ReflectedVariadicFields{
		number: 42
		label:  'field'
		items:  [1, 2]
		nested: [[3, 4], [5, 6]]
	}
	assert collect_reflected_fields(object) == ['[42]', "['field']", '[[1, 2]]', '[[[3, 4], [5, 6]]]']
}

fn spread_reflected_array_fields[T](object T) []string {
	mut output := []string{}
	$for field in T.fields {
		$if field.typ is $array {
			input := object.$(field.name)
			values := reflected_collect(...input)
			assert values == object.$(field.name)
			output << values.str()
		}
	}
	return output
}

fn test_reflected_array_fields_infer_spread_variadic_elements() {
	object := ReflectedVariadicFields{
		items:  [1, 2]
		nested: [[3, 4], [5, 6]]
	}
	assert spread_reflected_array_fields(object) == ['[1, 2]', '[[3, 4], [5, 6]]']
}
