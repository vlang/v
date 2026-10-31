struct CustomText[T] {
	value T
}

fn (text &CustomText[T]) str() string {
	return 'custom:${text.value}'
}

struct PlainText {
	value int
}

fn (text &PlainText) str() string {
	return 'plain:${text.value}'
}

struct AutomaticText[T] {
	value T
}

fn custom_text_pointer() &CustomText[int] {
	return &CustomText[int]{ value: 7 }
}

fn test_generic_reference_custom_str_call_preserves_the_method_result() {
	integer := &CustomText[int]{ value: 1 }
	text := &CustomText[string]{ value: 'hello' }
	plain := &PlainText{ value: 2 }
	assert integer.str() == 'custom:1'
	assert text.str() == 'custom:hello'
	assert plain.str() == 'plain:2'
	assert custom_text_pointer().str() == 'custom:7'
}

fn test_generic_reference_interpolation_keeps_the_reference_prefix() {
	integer := &CustomText[int]{ value: 1 }
	assert '${integer}' == '&custom:1'
	automatic := &AutomaticText[int]{ value: 3 }
	assert automatic.str().starts_with('&AutomaticText[')
}

fn test_generic_reference_custom_str_call_keeps_the_nil_guard() {
	text := unsafe { &CustomText[int](nil) }
	assert text.str() == '&nil'
}
