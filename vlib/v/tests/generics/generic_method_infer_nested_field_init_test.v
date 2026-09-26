struct ApiSuccessResponse[T] {
	data T
}

fn json_success[T](input ApiSuccessResponse[T]) ApiSuccessResponse[T] {
	return input
}

struct FieldInitContext {}

fn (ctx FieldInitContext) json[T](_value T) string {
	return 'ok'
}

fn test_generic_method_infers_from_nested_call_field_init_shorthand() {
	ctx := FieldInitContext{}
	result := 42
	assert ctx.json(json_success(data: result)) == 'ok'
}

fn test_generic_method_infers_from_nested_call_field_init_variable() {
	ctx := FieldInitContext{}
	result := 'hello'
	response := json_success(data: result)
	assert ctx.json(response) == 'ok'
}

fn test_generic_method_infers_from_nested_call_field_init_literals() {
	ctx := FieldInitContext{}
	assert ctx.json(json_success(data: 42)) == 'ok'
	assert ctx.json(json_success(data: 'hello')) == 'ok'
	assert ctx.json(json_success(data: 1.5)) == 'ok'
}

fn take_ptr[T](input &ApiSuccessResponse[T]) &ApiSuccessResponse[T] {
	return input
}

fn test_generic_method_infers_from_nested_call_field_init_pointer_param() {
	ctx := FieldInitContext{}
	assert ctx.json(take_ptr(data: 42)) == 'ok'
}

struct WrappedResponse[T] {
	inner ApiSuccessResponse[T]
}

fn wrap_response[T](input WrappedResponse[T]) WrappedResponse[T] {
	return input
}

fn test_generic_method_infers_from_nested_call_nested_generic_field() {
	ctx := FieldInitContext{}
	inner := ApiSuccessResponse[int]{
		data: 7
	}
	assert ctx.json(wrap_response(inner: inner)) == 'ok'
}
