// rate_limit_test.v - the rate limiter passes bursts and refuses floods.
//
// A token bucket per client key refills at the configured rate up to the
// burst size. The bucket math is pure (explicit millisecond timestamps, no
// threads), and the middleware is driven through `render_request`, where every
// request shares one store on the app. Without a limit, one client can spend
// the whole server; the first test below is that client.
module veb

import net.http
import time

struct LimitApp {
	Middleware[LimitContext]
}

struct LimitContext {
	Context
}

@['/api']
fn (app &LimitApp) api(mut ctx LimitContext) Result {
	return ctx.text('data')
}

// limit_request renders one GET through an app whose limiter was registered
// once, so all renders share the same buckets.
fn limit_request(mut app LimitApp, url string) &Context {
	return render_request[LimitApp, LimitContext](mut app, http.Request{
		method: .get
		url:    url
	}) or { panic(err) }
}

fn test_a_burst_of_requests_passes() {
	mut app := LimitApp{}
	app.use(rate_limit[LimitContext](requests_per_second: 1, burst: 3))
	for _ in 0 .. 3 {
		ctx := limit_request(mut app, '/api')
		assert ctx.res.status_code == int(http.Status.ok)
		assert ctx.res.body == 'data'
	}
}

fn test_the_request_past_the_burst_is_refused_with_retry_after() {
	mut app := LimitApp{}
	app.use(rate_limit[LimitContext](requests_per_second: 1, burst: 2))
	limit_request(mut app, '/api')
	limit_request(mut app, '/api')
	// All renders here share the test second, far below one token of refill.
	ctx := limit_request(mut app, '/api')
	assert ctx.res.status_code == int(http.Status.too_many_requests)
	assert ctx.res.body == '429 Too Many Requests'
	retry_after := ctx.res.header.get_custom('Retry-After') or {
		assert false, 'a refused request must carry Retry-After'
		return
	}
	assert retry_after.int() >= 1
}

fn test_a_custom_message_reaches_the_refused_client() {
	mut app := LimitApp{}
	app.use(rate_limit[LimitContext](
		requests_per_second: 1
		burst:               1
		message:             'slow down'
	))
	limit_request(mut app, '/api')
	ctx := limit_request(mut app, '/api')
	assert ctx.res.status_code == int(http.Status.too_many_requests)
	assert ctx.res.body == 'slow down'
}

fn test_buckets_refill_with_time() {
	mut store := new_rate_limit_store()
	options := RateLimitOptions{
		requests_per_second: 1
		burst:               1
	}
	allowed, _ := store.allow('client', 0, options)
	assert allowed
	allowed_again, retry_after := store.allow('client', 100, options)
	assert !allowed_again
	assert retry_after >= 1
	// A second later the bucket holds a whole token again.
	allowed_later, _ := store.allow('client', 1100, options)
	assert allowed_later
}

fn test_buckets_are_per_key() {
	mut store := new_rate_limit_store()
	options := RateLimitOptions{
		requests_per_second: 1
		burst:               1
	}
	allowed, _ := store.allow('first', 0, options)
	assert allowed
	_, _ = store.allow('first', 0, options)
	// `second` has its own full bucket: one's flood is not the other's.
	allowed_second, _ := store.allow('second', 0, options)
	assert allowed_second
	denied_first, _ := store.allow('first', 0, options)
	assert !denied_first
}

fn test_idle_keys_are_swept_past_the_cap() {
	mut store := new_rate_limit_store()
	options := RateLimitOptions{
		requests_per_second: 1
		burst:               1
		max_keys:            2
		idle_timeout:        time.minute
	}
	_, _ = store.allow('old', 0, options)
	_, _ = store.allow('recent', 59000, options)
	// Three keys with a cap of two sweeps the idle one; `recent` was touched
	// a second before `now` and survives, while `old` has been idle longer
	// than the minute timeout.
	_, _ = store.allow('new', 60001, options)
	assert 'old' !in store.buckets
	assert 'recent' in store.buckets
	assert 'new' in store.buckets
}
