module veb

struct MiddlewareAliasContext {
mut:
	steps []string
}

struct MiddlewareAliasCounter {
mut:
	calls int
}

fn middleware_alias_first(mut ctx MiddlewareAliasContext) bool {
	ctx.steps << 'first'
	return true
}

fn middleware_alias_stop(mut ctx MiddlewareAliasContext) bool {
	ctx.steps << 'stop'
	return false
}

fn middleware_alias_last(mut ctx MiddlewareAliasContext) bool {
	ctx.steps << 'last'
	return true
}

fn middleware_alias_count(mut ctx MiddlewareAliasCounter) bool {
	ctx.calls++
	return true
}

fn middleware_alias_after(mut ctx MiddlewareAliasContext) bool {
	ctx.steps << 'after'
	return true
}

fn invoke_middleware_alias[T](handler MiddlewareHandler[T], mut ctx T) bool {
	return handler(mut ctx)
}

fn test_middleware_handler_alias_preserves_mutable_context_types() {
	mut ctx := MiddlewareAliasContext{}
	mut counter := MiddlewareAliasCounter{}
	first := MiddlewareHandler[MiddlewareAliasContext](middleware_alias_first)
	count := MiddlewareHandler[MiddlewareAliasCounter](middleware_alias_count)

	assert invoke_middleware_alias[MiddlewareAliasContext](first, mut ctx)
	assert invoke_middleware_alias[MiddlewareAliasCounter](count, mut counter)
	assert invoke_middleware_alias[MiddlewareAliasContext](first, mut ctx)
	assert ctx.steps == ['first', 'first']
	assert counter.calls == 1
}

fn test_middleware_handler_alias_round_trips_through_voidptr() {
	mut ctx := MiddlewareAliasContext{}
	mut counter := MiddlewareAliasCounter{}

	// Exercise the same generic alias cast used by validate_middleware.
	first := MiddlewareHandler[MiddlewareAliasContext](voidptr(middleware_alias_first))
	count := MiddlewareHandler[MiddlewareAliasCounter](voidptr(middleware_alias_count))
	assert first(mut ctx)
	assert count(mut counter)
	assert ctx.steps == ['first']
	assert counter.calls == 1
}

fn test_middleware_handler_alias_empty_chain_continues() {
	mut ctx := MiddlewareAliasContext{}
	assert validate_middleware(mut ctx, []voidptr{})
	assert ctx.steps.len == 0
}

fn test_middleware_handler_alias_chain_stops_on_false() {
	mut ctx := MiddlewareAliasContext{}
	handlers := [voidptr(middleware_alias_first), voidptr(middleware_alias_stop),
		voidptr(middleware_alias_last)]
	assert !validate_middleware(mut ctx, handlers)
	assert ctx.steps == ['first', 'stop']
}

fn test_middleware_handler_alias_global_registration_keeps_order() {
	mut middleware := Middleware[MiddlewareAliasContext]{}
	middleware.use(handler: middleware_alias_first)
	middleware.use(handler: middleware_alias_last)
	middleware.use(handler: middleware_alias_after, after: true)
	mut ctx := MiddlewareAliasContext{}

	assert middleware.get_global_handlers().len == 2
	assert middleware.get_global_handlers_after().len == 1
	assert validate_middleware(mut ctx, middleware.get_global_handlers())
	assert ctx.steps == ['first', 'last']
	assert validate_middleware(mut ctx, middleware.get_global_handlers_after())
	assert ctx.steps == ['first', 'last', 'after']
}

fn test_middleware_handler_alias_route_registration_filters_methods() {
	mut middleware := Middleware[MiddlewareAliasContext]{}
	middleware.route_use('/alias', handler: middleware_alias_first, methods: [.get])
	middleware.route_use('/alias',
		handler: middleware_alias_last
		methods: [.get]
		after:   true
	)
	mut ctx := MiddlewareAliasContext{}
	handlers := middleware.get_handlers_for_route('/alias')
	after_handlers := middleware.get_handlers_for_route_after('/alias')

	assert handlers.len == 1
	assert after_handlers.len == 1
	assert get_handlers_for_method(handlers, .post).len == 0
	assert get_handlers_for_method(after_handlers, .post).len == 0
	assert validate_middleware(mut ctx, get_handlers_for_method(handlers, .get))
	assert validate_middleware(mut ctx, get_handlers_for_method(after_handlers, .get))
	assert ctx.steps == ['first', 'last']
}
