struct HeapMutArgMarker {
	frame int
}

@[heap]
struct HeapMutArgApp {
mut:
	calls int
}

fn heap_mut_arg_markers(mut app HeapMutArgApp) []HeapMutArgMarker {
	app.calls++
	return [HeapMutArgMarker{app.calls}, HeapMutArgMarker{app.calls + 10}]
}

// The lowering declares a `@[heap]` local again, as a reference. It stays `mut`
// there, so `f(mut app)` is still accepted where it is the receiver of `last()`.
fn test_mut_heap_local_as_argument_of_a_call_receiver() {
	mut app := HeapMutArgApp{}
	assert heap_mut_arg_markers(mut app).last().frame == 11
	assert heap_mut_arg_markers(mut app).first().frame == 2
	assert app.calls == 2
}
