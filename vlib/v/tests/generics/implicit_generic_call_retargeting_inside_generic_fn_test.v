// Regression coverage for implicit nested generic calls in a generic body that is instantiated with
// several outer types.
//
// The template node is shared between instances, so a nested callee may already name the
// specialization chosen while transforming an earlier instance. Every later instance must still be
// retargeted, and that must stay true alongside the pinning of explicit `fn_name[Type]` callees.

fn tag[X](_x X) string {
	return typeof[X]().name
}

fn produce[X]() X {
	return X{}
}

// Both spellings target the same callee from one shared template: the explicit call is pinned to
// `string` for every instance, while the implicit one has to follow `T`.
fn mixed[T](v T) string {
	explicit := tag[string]('literal')
	implicit := tag(v)
	return '${explicit}/${implicit}'
}

fn test_implicit_call_retargets_while_explicit_call_stays_pinned() {
	assert mixed(1) == 'string/int'
	assert mixed(2.5) == 'string/f64'
	assert mixed('s') == 'string/string'
}

// The nested call takes no arguments, so its type argument comes from the enclosing return type and
// changes with every instance.
fn relay[T]() T {
	return produce()
}

fn test_argumentless_implicit_call_follows_each_instance() {
	assert relay[int]() == 0
	assert relay[string]() == ''
	assert relay[f64]() == 0.0
}

// Same shape behind a closure, which is the case the retarget path calls out explicitly.
fn closured[T](v T) string {
	f := fn [v] [T]() string {
		return tag(v)
	}
	return f()
}

fn test_implicit_call_inside_cloned_closure_retargets() {
	assert closured(1) == 'int'
	assert closured('s') == 'string'
	assert closured(2.5) == 'f64'
}
