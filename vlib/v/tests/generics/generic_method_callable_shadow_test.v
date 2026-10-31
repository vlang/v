struct CallableShadowBox[T] {
	value T
}

fn (b CallableShadowBox[T]) show(show fn (T) string) string {
	return show(b.value)
}

fn (b CallableShadowBox[T]) local(callback fn (T) string) string {
	show := callback
	return show(b.value)
}

fn (b CallableShadowBox[T]) describe(value T) string {
	return 'method:${value}'
}

fn (b CallableShadowBox[T]) implicit() string {
	return describe(b.value)
}

fn show_shadow_int(value int) string {
	return 'int:${value}'
}

fn test_generic_method_callable_parameter_shadows_method() {
	box := CallableShadowBox[int]{ value: 4 }
	assert box.show(show_shadow_int) == 'int:4'
	assert box.show(fn (value int) string {
		return 'closure:${value}'
	}) == 'closure:4'
	text := CallableShadowBox[string]{ value: 'hello' }
	assert text.show(fn (value string) string {
		return value.to_upper()
	}) == 'HELLO'
}

fn test_generic_method_local_callable_shadows_method() {
	box := CallableShadowBox[int]{ value: 7 }
	assert box.local(show_shadow_int) == 'int:7'
}

fn test_generic_method_implicit_receiver_call_remains_supported() {
	box := CallableShadowBox[int]{ value: 9 }
	assert box.implicit() == 'method:9'
}
