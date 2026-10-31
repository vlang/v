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

fn test_closed_capacity_uses_the_configured_response_and_retry_header() {
	mut app := LimitApp{}
	app.use(rate_limit[LimitContext](max_keys: 0, message: 'no client slots'))
	ctx := limit_request(mut app, '/api')
	assert ctx.res.status_code == int(http.Status.too_many_requests)
	assert ctx.res.body == 'no client slots'
	retry_after := ctx.res.header.get_custom('Retry-After') or {
		assert false, 'a full store must carry Retry-After'
		return
	}
	assert retry_after == '60'
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

fn test_active_clients_cannot_grow_the_table_past_the_cap() {
	mut store := new_rate_limit_store()
	options := RateLimitOptions{
		requests_per_second: 1
		burst:               1
		max_keys:            2
		idle_timeout:        time.minute
	}
	for i in 0 .. 20 {
		allowed, retry_after := store.allow('client-${i}', 0, options)
		assert allowed == (i < options.max_keys)
		if i >= options.max_keys {
			assert retry_after == 61
		}
		assert store.buckets.len <= options.max_keys
	}
	// Refusing new clients must not evict a depleted bucket or reset its burst.
	allowed, retry_after := store.allow('client-0', 0, options)
	assert !allowed
	assert retry_after >= 1
	assert store.buckets['client-0'].tokens == 0
	assert store.buckets.len == options.max_keys
	refilled, _ := store.allow('client-0', 1000, options)
	assert refilled
	assert store.buckets.len == options.max_keys
}

fn test_full_table_retries_when_the_earliest_bucket_can_expire() {
	mut store := new_rate_limit_store()
	options := RateLimitOptions{
		requests_per_second: 1
		burst:               1
		max_keys:            2
		idle_timeout:        1500 * time.millisecond
	}
	_, _ = store.allow('first', 1000, options)
	_, _ = store.allow('second', 1500, options)
	allowed_earlier, retry_earlier := store.allow('new', 1500, options)
	assert !allowed_earlier
	assert retry_earlier == 2
	allowed, retry_after := store.allow('new', 2000, options)
	assert !allowed
	assert retry_after == 1
	assert 'new' !in store.buckets
	// Exactly the timeout is still occupied; expiry requires longer idle time.
	allowed_at_timeout, retry_at_timeout := store.allow('new', 2500, options)
	assert !allowed_at_timeout
	assert retry_at_timeout == 1
	allowed_after_timeout, _ := store.allow('new', 2501, options)
	assert allowed_after_timeout
	assert 'first' !in store.buckets
	assert 'second' in store.buckets
	assert store.buckets.len == options.max_keys
}

fn test_nonpositive_max_keys_reject_without_allocating_buckets() {
	for max_keys in [0, -1] {
		mut store := new_rate_limit_store()
		options := RateLimitOptions{
			max_keys: max_keys
		}
		for i in 0 .. 20 {
			allowed, retry_after := store.allow('client-${i}', 0, options)
			assert !allowed
			assert retry_after == 60
		}
		assert store.buckets.len == 0
	}
}

fn test_requests_without_refill_keep_their_bucket_active() {
	mut store := new_rate_limit_store()
	options := RateLimitOptions{
		requests_per_second: 0
		burst:               1
		max_keys:            1
		idle_timeout:        time.second
	}
	allowed, _ := store.allow('active', 0, options)
	assert allowed
	allowed_again, _ := store.allow('active', 1000, options)
	assert !allowed_again
	allowed_new, retry_after := store.allow('new', 1001, options)
	assert !allowed_new
	assert retry_after == 1
	assert 'active' in store.buckets
	assert store.buckets['active'].tokens == 0
}
