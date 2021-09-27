import context

const not_found_value = &Value{
	val: 'key not found'
}

struct Value {
	val string
}

// This example demonstrates how a value can be passed to the context
// and also how to retrieve it if it exists.
fn test_with_value() {
	f := fn (ctx context.Context, key context.Key) &Value {
		if value := ctx.value(key) {
			match value {
				Value {
					return value
				}
				else {}
			}
		}
		return not_found_value
	}

	key := 'language'
	value := &Value{
		val: 'VAL'
	}
	ctx := context.with_value(context.background(), key, value)

	assert value == f(ctx, key)
	assert not_found_value == f(ctx, 'color')
}
