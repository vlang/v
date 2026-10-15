module transform

fn test_variadic_inference_preserves_nested_array_element_types() {
	for param, argument in {
		'...T':     '[][]int'
		'...[]T':   '[][][]int'
		'...[][]T': '[][][][]int'
	} {
		mut inferred := map[string]string{}
		infer_generic_type_args(param, argument, mut inferred)
		assert inferred['T'] == '[]int'
	}
}

fn test_single_array_argument_infers_variadic_array_element() {
	mut inferred := map[string]string{}
	infer_generic_type_args('...[]T', '[]int', mut inferred)
	assert inferred['T'] == 'int'
}
