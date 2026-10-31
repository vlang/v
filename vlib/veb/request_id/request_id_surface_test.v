module request_id

import veb

// Coverage for the parts of the request_id middleware that `request_id_test.v`
// leaves untouched: the existing-header path, `get_request_id`, and the config
// the handler stores on the context.
//
// A zero `veb.Context` is enough here -- nothing in the handler reaches the
// socket, and both header maps it reads and writes are plain map fields.

struct RidCtx {
	veb.Context
	RequestIdContext
}

fn run_rid(mut ctx RidCtx, cfg Config) bool {
	return middleware[RidCtx](cfg).handler(mut ctx)
}

fn test_get_request_id_returns_the_stored_value() {
	mut ctx := RidCtx{}
	assert ctx.get_request_id() == ''
	ctx.request_id = 'set-directly'
	assert ctx.get_request_id() == 'set-directly'
}

fn test_get_request_id_reflects_what_the_handler_stored() {
	mut ctx := RidCtx{}
	run_rid(mut ctx, Config{
		generator: fn () string {
			return 'generated'
		}
	})
	assert ctx.get_request_id() == 'generated'
}

fn test_handler_keeps_a_request_id_the_client_sent() {
	mut ctx := RidCtx{}
	ctx.req.header.add_custom('X-Request-ID', 'from-client') or {
		assert false, 'add_custom failed: ${err}'
		return
	}
	run_rid(mut ctx, Config{
		generator: fn () string {
			return 'generated'
		}
	})
	assert ctx.request_id == 'from-client'
	assert ctx.get_request_id() == 'from-client'
}

fn test_handler_reads_the_header_named_in_the_config() {
	mut ctx := RidCtx{}
	ctx.req.header.add_custom('Trace-Id', 'abc') or {
		assert false, 'add_custom failed: ${err}'
		return
	}
	run_rid(mut ctx, Config{
		header:    'Trace-Id'
		generator: fn () string {
			return 'generated'
		}
	})
	assert ctx.get_request_id() == 'abc'
	// The response header is written under the same key, on the *response* map.
	assert ctx.res.header.get_custom('Trace-Id') or { '' } == 'abc'
}

fn test_handler_writes_the_response_header_and_stores_the_config() {
	mut ctx := RidCtx{}
	run_rid(mut ctx, Config{
		header:    'Rid-Key'
		generator: fn () string {
			return 'rid'
		}
	})
	// NOTE: `set_custom_header` writes to the *response* header map, which
	// `get_custom_header` does not read -- it only sees the request's.
	assert ctx.res.header.get_custom('Rid-Key') or { '' } == 'rid'
	assert ctx.get_custom_header('Rid-Key') or { '' } == ''
	assert ctx.request_id_config.header == 'Rid-Key'
	assert ctx.request_id_config.force == false
}

fn test_handler_generates_when_the_client_sent_no_header() {
	mut ctx := RidCtx{}
	run_rid(mut ctx, Config{
		generator: fn () string {
			return 'fresh'
		}
	})
	assert ctx.get_request_id() == 'fresh'
	assert ctx.request_id_exempt == false
}

fn test_force_regenerates_even_when_the_client_sent_one() {
	mut ctx := RidCtx{}
	ctx.req.header.add_custom('X-Request-ID', 'from-client') or {
		assert false, 'add_custom failed: ${err}'
		return
	}
	run_rid(mut ctx, Config{
		force:     true
		generator: fn () string {
			return 'forced'
		}
	})
	assert ctx.get_request_id() == 'forced'
}

fn test_exempt_short_circuits_before_any_header_is_read() {
	mut ctx := RidCtx{
		request_id_exempt: true
	}
	ctx.req.header.add_custom('X-Request-ID', 'from-client') or {
		assert false, 'add_custom failed: ${err}'
		return
	}
	// The generator is not called, so a handler that runs it would be observed.
	run_rid(mut ctx, Config{
		generator: fn () string {
			assert false, 'generator ran for an exempt request'
			return ''
		}
	})
	assert ctx.get_request_id() == ''
}

// NOTE: `allow_empty` is declared in Config but the handler never reads it, so
// an empty id can never be produced -- the generator fills the gap first.
fn test_allow_empty_is_inert_in_the_handler() {
	cfg := Config{
		allow_empty: true
	}
	mut ctx := RidCtx{}
	run_rid(mut ctx, cfg)
	assert ctx.get_request_id() != ''
	assert ctx.request_id_config.allow_empty == true
}
