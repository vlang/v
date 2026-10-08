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

fn set_generic_local[T](mut value T, next T) {
	value = next
}

fn name_after_mut_call[T](sample T) string {
	_ = sample
	name := ''
	set_generic_local(mut name, 'changed')
	if name == '' {
		return 'none'
	}
	return name
}

fn count_after_mut_call[T](sample T) int {
	_ = sample
	count := 0
	set_generic_local(mut count, 3)
	if count == 0 {
		return -1
	}
	return count
}

fn test_mutable_call_local_of_generic_fn_is_not_folded() {
	assert name_after_mut_call(1) == 'changed'
	assert count_after_mut_call('sample') == 3
}

fn string_and_count() (string, int) {
	return 'changed', 3
}

fn name_after_multi_assign[T](sample T) string {
	_ = sample
	name := ''
	other := 'kept'
	mut count := 0
	// `name` is a target in the second position, `other` is only read.
	count, name = 2, other + '!'
	if name == '' {
		return 'none'
	}
	return '${name}${count}'
}

fn name_after_call_assign[T](sample T) string {
	_ = sample
	name := ''
	mut count := 0
	name, count = string_and_count()
	if name == '' {
		return 'none'
	}
	return '${name}${count}'
}

fn test_multi_assigned_local_of_generic_fn_is_not_folded() {
	assert name_after_multi_assign(1) == 'kept!2'
	assert name_after_call_assign('sample') == 'changed3'
}

// Reading an immutable local on the right of an assignment leaves it a compile-time value.
fn test_local_read_by_an_assignment_stays_a_compile_time_value() {
	source := 'a b'
	mut copied := ''
	copied = source
	mut words := []string{}
	$for word in source.fields() {
		words << word
	}
	assert copied == 'a b'
	assert words == ['a', 'b']

	mut seen := []string{}
	$for method in 'get post'.fields() {
		current := method
		mut last := ''
		last = current
		$if current == 'get' {
			seen << 'first:' + last
		} $else {
			seen << 'other:' + last
		}
	}
	assert seen == ['first:get', 'other:post']
}
