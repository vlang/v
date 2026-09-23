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
