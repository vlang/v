// Regression coverage for explicit type arguments on a generic call that is made from inside
// another generic function.
//
// The enclosing function's return type (and its own generic arguments) must never replace an
// explicit `fn_name[Type]` list when the nested call has nothing to infer from.

type Sum = int | string

struct Box {
	v int
}

fn make_it[X]() X {
	return X{}
}

fn make_from_string[X](_s string) X {
	return X{}
}

fn make_result[X](_s string) !X {
	return X{}
}

fn make_option[X](_s string) ?X {
	return X{}
}

// The enclosing generic parameter is deliberately named `T` here and `X` in the callees, and
// `same_name_param` reuses `T` for both, so a name collision cannot leak the caller's argument.
fn same_name_param[T]() T {
	return T{}
}

fn outer_returns_string[T](_unused T) string {
	v := make_it[Sum]()
	return typeof(v).name
}

fn outer_returns_box[T](_unused T) string {
	v := make_it[Box]()
	return typeof(v).name
}

fn outer_with_arg[T](_unused T) string {
	v := make_from_string[Sum]('x')
	return typeof(v).name
}

fn outer_with_result[T](_unused T) string {
	v := make_result[Sum]('x') or { return 'failed' }
	return typeof(v).name
}

fn outer_with_option[T](_unused T) string {
	v := make_option[Sum]('x') or { return 'failed' }
	return typeof(v).name
}

fn outer_same_generic_name[T](_unused T) string {
	v := same_name_param[Sum]()
	return typeof(v).name
}

fn test_explicit_type_args_survive_inside_generic_fn() {
	assert outer_returns_string(1) == 'Sum'
	assert outer_returns_string('s') == 'Sum'
	assert outer_returns_string(Box{}) == 'Sum'
	assert outer_returns_box(1) == 'Box'
	assert outer_with_arg(1) == 'Sum'
	assert outer_with_result(1) == 'Sum'
	assert outer_with_option(1) == 'Sum'
}

fn test_explicit_type_args_ignore_matching_generic_param_names() {
	assert outer_same_generic_name(1) == 'Sum'
	assert outer_same_generic_name('s') == 'Sum'
}

// A nested call without explicit type arguments still has to be re-inferred for every
// specialization of its caller.
fn passthrough[T]() T {
	return make_it()
}

fn test_implicit_nested_generic_calls_are_still_retargeted() {
	assert passthrough[int]() == 0
	assert passthrough[string]() == ''
	assert typeof(passthrough[Box]()).name == 'Box'
}
