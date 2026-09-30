import fnaliascontext

type Handler[T] = fn (mut T) string

type Mapper[T] = fn (T) string

struct Context {
mut:
	message string
}

fn update_context(mut ctx Context) bool {
	ctx.message += '!'
	return true
}

fn update_imported_context(mut ctx fnaliascontext.Context) bool {
	ctx.value++
	return true
}

fn map_context(ctx Context) Context {
	return Context{ message: ctx.message + '?' }
}

fn map_imported_context(ctx fnaliascontext.Context) fnaliascontext.Context {
	return fnaliascontext.Context{ value: ctx.value + 10 }
}

fn test_generic_fn_alias_cast_preserves_mut_parameter_module() {
	mut ctx := Context{ message: 'ok' }
	assert fnaliascontext.apply[Context](mut ctx, voidptr(update_context))
	assert ctx.message == 'ok!'
	mut imported_ctx := fnaliascontext.Context{ value: 7 }
	assert fnaliascontext.apply[fnaliascontext.Context](mut imported_ctx, voidptr(update_imported_context))
	assert imported_ctx.value == 8
}

fn test_generic_fn_alias_cast_preserves_value_parameter_and_return_module() {
	ctx := fnaliascontext.map[Context](Context{ message: 'ok' }, voidptr(map_context))
	assert ctx.message == 'ok?'
	imported_ctx := fnaliascontext.map[fnaliascontext.Context](fnaliascontext.Context{ value: 7 },
		voidptr(map_imported_context))
	assert imported_ctx.value == 17
}

fn test_main_fn_alias_cast_keeps_its_own_signature() {
	mut ctx := Context{ message: 'main' }
	handler := Handler[Context](fn (mut value Context) string {
		return value.message
	})
	mapper := Mapper[Context](fn (value Context) string {
		return value.message + '?'
	})
	assert handler(mut ctx) == 'main'
	assert mapper(ctx) == 'main?'
}
