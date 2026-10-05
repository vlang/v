// A bare identifier in an array literal must be typed from the binding that shadows
// its name, not from a module-level function of the same name. `for` asks the
// checker for the literal's type after checking has moved on, at which point the
// enclosing parameter is no longer reachable through the scope chain, so the
// literal was typed as an array of function pointers and the C backend emitted
// incompatible assignments:
//
//	error: cannot convert 'struct string' to
//	            'struct string (*)(struct main__FlatAst *, struct main__Node)'
//
// `vlib/v/transform/fn.v` reads `for name in [call_name, c_name(call_name)]`, and
// `vlib/v/compiler_tests/struct_default_transform_test.v` declares a function called
// `call_name`, so that test could not be built at all.
module main

struct FlatAst {
	n int
}

struct Node {
	n int
}

fn call_name(a &FlatAst, call Node) string {
	return 'a'
}

fn same_sig(a &FlatAst, call Node) string {
	return 'b'
}

fn c_name(s string) string {
	return s
}

struct Holder {
	f string
}

// The parameter shadows the function above.
fn shadowed_param(call_name string) []string {
	mut out := []string{}
	for x in [call_name, c_name(call_name)] {
		out << x
	}
	return out
}

fn shadowed_param_single(call_name string) []string {
	mut out := []string{}
	for x in [call_name] {
		out << x
	}
	return out
}

fn shadowed_param_first(call_name string) []string {
	mut out := []string{}
	for x in [call_name, call_name] {
		out << x
	}
	return out
}

fn shadowed_local() []string {
	call_name := 'local'
	mut out := []string{}
	for x in [call_name, c_name(call_name)] {
		out << x
	}
	return out
}

fn unrelated_name(other string) []string {
	mut out := []string{}
	for x in [other, c_name(other)] {
		out << x
	}
	return out
}

fn field_shadowing(h Holder) []string {
	mut out := []string{}
	for x in [h.f, c_name(h.f)] {
		out << x
	}
	return out
}

fn no_shadowing_same_signature() int {
	mut n := 0
	for _ in [call_name, same_sig] {
		n++
	}
	return n
}

fn test_shadowed_param_in_for_literal() {
	assert shadowed_param('p') == ['p', 'p']
}

fn test_shadowed_param_single_element() {
	assert shadowed_param_single('p') == ['p']
}

fn test_shadowed_param_repeated() {
	assert shadowed_param_first('p') == ['p', 'p']
}

fn test_shadowed_local() {
	assert shadowed_local() == ['local', 'local']
}

fn test_unrelated_name_is_unaffected() {
	assert unrelated_name('u') == ['u', 'u']
}

fn test_field_named_like_the_function() {
	assert field_shadowing(Holder{
		f: 'h'
	}) == ['h', 'h']
}

fn test_array_of_functions_still_works() {
	assert no_shadowing_same_signature() == 2
}