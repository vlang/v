module request_id

import veb

// Test context that includes our RequestIdContext
struct TestContext {
	veb.Context
	RequestIdContext
}

fn test_config_default() {
	default_config := Config{}
	assert default_config.header == 'X-Request-ID'
	assert default_config.allow_empty == false
	assert default_config.force == false
	assert default_config.next == none
	assert default_config.generator != unsafe { nil }
}

fn test_middleware_handler() {
	cfg := Config{
		header:    'Test-Request-ID'
		generator: fn () string {
			return 'test-123'
		}
	}

	// Create middleware handler
	handler := middleware[TestContext](cfg).handler

	// Create test context
	mut ctx := TestContext{}

	// Test handler execution
	result := handler(mut ctx)
	assert result == true

	// Verify request ID was set
	assert ctx.request_id == 'test-123'
}

fn test_middleware_next_function() {
	mut ctx := TestContext{}

	// Test with next function that returns true
	skip_cfg := Config{
		next: fn (ctx &veb.Context) bool {
			return true
		}
	}
	skip_handler := middleware[TestContext](skip_cfg).handler

	result := skip_handler(mut ctx)
	assert result == true
	assert ctx.request_id == ''

	// Test with next function that returns false
	continue_cfg := Config{
		next:      fn (ctx &veb.Context) bool {
			return false
		}
		generator: fn () string {
			return 'test-123'
		}
	}
	continue_handler := middleware[TestContext](continue_cfg).handler

	result2 := continue_handler(mut ctx)
	assert result2 == true
	assert ctx.request_id == 'test-123'
}

fn test_middleware_force_option() {
	mut ctx := TestContext{}
	ctx.request_id = 'existing-id'

	force_cfg := Config{
		force:     true
		generator: fn () string {
			return 'forced-id'
		}
	}
	force_handler := middleware[TestContext](force_cfg).handler

	result := force_handler(mut ctx)
	assert result == true
	assert ctx.request_id == 'forced-id'
}

fn test_middleware_exempt() {
	mut ctx := TestContext{
		request_id_exempt: true
	}

	handler := middleware[TestContext](Config{}).handler

	result := handler(mut ctx)
	assert result == true
	assert ctx.request_id == ''
}
