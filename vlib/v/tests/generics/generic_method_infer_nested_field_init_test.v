struct ApiSuccessResponse[T] {
	data T
}

type ApiSuccessResponseRef = &ApiSuccessResponse[int]
type ApiSuccessResponseRef2 = ApiSuccessResponseRef
type ApiSuccessResponseRef3 = ApiSuccessResponseRef2
type ApiSuccessResponseRef4 = ApiSuccessResponseRef3
type ApiSuccessResponseRef5 = ApiSuccessResponseRef4
type ApiSuccessResponseRef6 = ApiSuccessResponseRef5
type ApiSuccessResponseRef7 = ApiSuccessResponseRef6
type ApiSuccessResponseRef8 = ApiSuccessResponseRef7
type ApiSuccessResponseRef9 = ApiSuccessResponseRef8

fn json_success[U](input ApiSuccessResponse[U]) ApiSuccessResponse[U] {
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

fn take_ptr[U](input &ApiSuccessResponse[U]) &ApiSuccessResponse[U] {
	return input
}

fn read_ptr[U](input &ApiSuccessResponse[U]) U {
	return input.data
}

fn read_alias_ptr(input ApiSuccessResponseRef9) int {
	return input.data
}

fn first_pointer_variadic[T](items ...&ApiSuccessResponse[T]) T {
	return items[0].data
}

fn first_int_pointer_variadic(items ...&ApiSuccessResponse[int]) int {
	return items[0].data
}

fn second_pointer_variadic[T](items ...&ApiSuccessResponse[T]) T {
	return items[1].data
}

fn test_generic_method_infers_from_nested_call_field_init_pointer_param() {
	ctx := FieldInitContext{}
	assert ctx.json(take_ptr(data: 42)) == 'ok'
	assert read_ptr(data: 42) == 42
	retained := take_ptr(data: 42)
	assert retained.data == 42
	assert read_alias_ptr(data: 43) == 43
	assert first_pointer_variadic(data: 45) == 45
	assert first_int_pointer_variadic(data: 46) == 46
	second := &ApiSuccessResponse[int]{ data: 47 }
	assert second_pointer_variadic(data: 45, second) == 47
}

struct WrappedResponse[T] {
	inner ApiSuccessResponse[T]
}

fn wrap_response[U](input WrappedResponse[U]) WrappedResponse[U] {
	return input
}

fn test_generic_method_infers_from_nested_call_nested_generic_field() {
	ctx := FieldInitContext{}
	inner := ApiSuccessResponse[int]{
		data: 7
	}
	assert ctx.json(wrap_response(inner: inner)) == 'ok'
}

struct ReorderedResponse[T, U] {
	data T
	tag  U
}

fn reordered_response[T, U](input ReorderedResponse[U, T], tag T) ReorderedResponse[U, T] {
	return ReorderedResponse[U, T]{
		data: input.data
		tag:  tag
	}
}

fn test_generic_method_infers_reordered_struct_parameters_from_nested_field_init() {
	ctx := FieldInitContext{}
	result := 42
	assert ctx.json(reordered_response(data: result, 'tag')) == 'ok'
	response := reordered_response(data: result, 'tag')
	assert response.data == 42
	assert response.tag == 'tag'
}

struct ArrayResponse[T] {
	data []T
}

fn array_response[U](input ArrayResponse[U]) ArrayResponse[U] {
	return input
}

fn test_generic_method_infers_array_fields_with_renamed_struct_parameters() {
	ctx := FieldInitContext{}
	values := [1, 2, 3]
	assert ctx.json(array_response(data: values)) == 'ok'
	assert ctx.json(array_response(data: [1, 2, 3])) == 'ok'
	response := array_response(data: ['first', 'second'])
	assert response.data == ['first', 'second']
}

fn (ctx FieldInitContext) response[U](input ApiSuccessResponse[U]) ApiSuccessResponse[U] {
	return input
}

fn (ctx FieldInitContext) reordered[T, U](input ReorderedResponse[U, T], tag T) ReorderedResponse[U, T] {
	return ReorderedResponse[U, T]{
		data: input.data
		tag:  tag
	}
}

fn test_nested_generic_method_field_init_substitutes_struct_parameters() {
	ctx := FieldInitContext{}
	assert ctx.json(ctx.response(data: 42)) == 'ok'
	response := ctx.response(data: 'hello')
	assert response.data == 'hello'
	assert ctx.json(ctx.reordered(data: 42, 'tag')) == 'ok'
	reordered := ctx.reordered(data: 42, 'tag')
	assert reordered.data == 42
	assert reordered.tag == 'tag'
}
