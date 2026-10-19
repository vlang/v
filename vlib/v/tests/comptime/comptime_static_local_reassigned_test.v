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

fn append_changed(mut values []string) {
	values << 'changed'
}

// A function literal has its own scope: a local declared there is another binding, even
// with the name of an outer one.
fn test_local_shadowed_in_a_function_literal_stays_a_compile_time_value() {
	source := 'a b'
	change_inner := fn () string {
		mut source := []string{}
		append_changed(mut source)
		return source[0]
	}
	assert change_inner() == 'changed'
	reassign_inner := fn () string {
		mut source := 'inner'
		source = 'reassigned'
		source += '!'
		return source
	}
	assert reassign_inner() == 'reassigned!'
	mut words := []string{}
	$for word in source.fields() {
		words << word
	}
	assert words == ['a', 'b']
}

// A binding of an earlier block is out of scope where the same name is declared again.
fn test_local_named_like_an_earlier_block_local_stays_a_compile_time_value() {
	mut earlier := ''
	if earlier == '' {
		mut source := 'first'
		source = 'second'
		earlier = source
	}
	source := 'a b'
	mut words := []string{}
	$for word in source.fields() {
		words << word
	}
	assert earlier == 'second'
	assert words == ['a', 'b']
}

// A captured scalar is a copy that belongs to the closure, so the outer local keeps its value.
fn name_after_mut_capture[T](sample T) string {
	_ = sample
	name := ''
	set := fn [mut name] () string {
		name = 'changed'
		return name
	}
	inner := set()
	if name == '' {
		return 'outer unchanged, inner ${inner}'
	}
	return name
}

fn test_mut_capture_of_generic_fn_local_leaves_the_outer_value() {
	assert name_after_mut_capture(1) == 'outer unchanged, inner changed'
}
