struct Model[T] {}

fn (model Model[T]) load() string {
	saved_type := 'Other'
	mut result := ''
	for j := 0; j < 1; j++ {
		weight_key := match saved_type {
			'LinearLayer' {
				if j == 0 { 'weight' } else { 'bias' }
			}
			'LayerNormLayer' {
				if j == 0 { 'gamma' } else { 'beta' }
			}
			else {
				'var_${j}'
			}
		}

		result = weight_key
	}
	return result
}

fn test_match_expr_nested_if_in_generic_method() {
	assert Model[int]{}.load() == 'var_0'
}
