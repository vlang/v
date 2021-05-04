import context

type ValueContextKey = string

// This example demonstrates how a value can be passed to the context
// and also how to retrieve it if it exists.
fn test_with_value() {
	f := fn (ctx context.Context, key ValueContextKey) string {
		if value := ctx.value(key) {
			if !isnil(value) {
				return *(&string(value))
			}
		}
		return 'key not found'
	}

	key := ValueContextKey('language')
	value := 'VAL'
	ctx := context.with_value(context.background(), key, &value)

	assert value == f(ctx, key)
	assert 'key not found' == f(ctx, ValueContextKey('color'))
}
