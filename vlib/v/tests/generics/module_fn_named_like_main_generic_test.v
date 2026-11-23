import json2

type Value = bool | voidptr

// `os`, which json2 imports, has `create(path string)` and calls it by its bare
// name. That call must not ask for `create[string]` of this program.
fn create[T](val T) Value {
	$if T is bool {
		return Value(val)
	} $else {
		$compile_error('create takes a bool')
	}
	return Value(voidptr(0))
}

fn test_module_call_named_like_a_main_generic_does_not_specialize_it() {
	_ := json2.Any(1)
	assert create(true) == Value(true)
}
