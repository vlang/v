// A local function value shadows a function of the same name: a call of it must use
// the local's own signature. With a variadic `join_parts` declared, the call of the
// local closure used to pack its arguments for the variadic function, and the C
// compiler rejected the call.
fn join_parts(parts ...string) string {
	return 'fn:' + parts.join('|')
}

fn test_local_closure_shadows_a_variadic_function() {
	join_parts := fn (a string, b string) string {
		return a + ':' + b
	}
	assert join_parts('a', 'b') == 'a:b'
}

fn test_parameter_shadows_a_variadic_function() {
	assert call_with(fn (a string, b string) string {
		return b + a
	}) == 'yx'
}

fn call_with(join_parts fn (string, string) string) string {
	return join_parts('x', 'y')
}

fn test_the_function_itself_stays_variadic() {
	assert join_parts() == 'fn:'
	assert join_parts('a', 'b', 'c') == 'fn:a|b|c'
	f := join_parts
	assert f('x') == 'fn:x'
}

// A function declared without a body cannot be called, but a local of the same
// name can.
fn bodyless_target(x int) int

fn test_local_closure_shadows_a_bodyless_function() {
	bodyless_target := fn (x int) int {
		return x * 2
	}
	assert bodyless_target(21) == 42
}
