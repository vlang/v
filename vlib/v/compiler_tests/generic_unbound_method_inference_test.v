struct GenericMethodCallbackFoo {}

fn (mut f GenericMethodCallbackFoo) value() int {
	return 42
}

fn (mut f GenericMethodCallbackFoo) call[T](run fn (mut GenericMethodCallbackFoo) T) T {
	return run(mut f)
}

fn test_unbound_instance_method_infers_generic_return_type() {
	mut f := GenericMethodCallbackFoo{}
	assert f.call(GenericMethodCallbackFoo.value) == 42
}
