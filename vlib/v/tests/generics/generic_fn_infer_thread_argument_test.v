// Regression test, sibling of https://github.com/vlang/v/issues/30842
//
// A generic fn with a `thread T` parameter, called without explicit type
// arguments, was rejected by the checker (`cannot use thread string as
// thread T`): `T` was not inferred from the thread handle's payload type,
// and the transformer/cgen unifiers had no `thread` arm either.

struct Msg {
	text string
}

fn wait_it[T](t thread T) T {
	return t.wait()
}

fn payload_default[T](_t thread T) T {
	return T{}
}

fn work_string() string {
	return 'w'
}

fn work_int(n int) int {
	return n * 2
}

fn work_msg(text string) Msg {
	return Msg{
		text: text
	}
}

fn test_implicit_thread_string() {
	t := spawn work_string()
	assert wait_it(t) == 'w'
}

fn test_implicit_thread_int() {
	t := spawn work_int(21)
	assert wait_it(t) == 42
	assert payload_default(spawn work_int(1)) == 0
}

fn test_implicit_thread_struct_payload() {
	t := spawn work_msg('hello')
	m := wait_it(t)
	assert m.text == 'hello'
	assert payload_default(spawn work_msg('x')).text == ''
}

fn test_explicit_thread_still_works() {
	t := spawn work_string()
	assert wait_it[string](t) == 'w'
}
