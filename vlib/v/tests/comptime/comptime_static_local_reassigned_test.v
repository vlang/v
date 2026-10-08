enum Color {
	red
	green
}

// A generic function body is not checked for assignments to a local declared without
// `mut`, so such a local has to stay a runtime value although its initializer is static.
fn variant_name[T](n int) string {
	name := ''
	$for variant in T.values {
		if int(variant.value) == n {
			name = variant.name
		}
	}
	if name == '' {
		return 'none'
	}
	return name
}

fn positive_count[T](values []T) int {
	count := 0
	for value in values {
		if value > 0 {
			count++
		}
	}
	if count == 0 {
		return -1
	}
	return count
}

fn test_reassigned_local_of_generic_fn_is_not_folded() {
	assert variant_name[Color](1) == 'green'
	assert variant_name[Color](7) == 'none'
	assert positive_count([1, -2, 3]) == 2
	assert positive_count([-1.5]) == -1
}
