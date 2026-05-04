import context

struct Ctx {
	ctx context.Context
}

fn smth_with_ctx(ctx context.Context) Ctx {
	return Ctx{
		ctx: ctx
	}
}

fn test_string_interpolation_of_struct_with_context_interface_field() {
	ctx := context.todo()
	rslt := smth_with_ctx(ctx)
	s := '${rslt}'
	assert s.starts_with('Ctx{')
	assert s.contains('ctx:')
	assert s.contains('context.TODO')
}
