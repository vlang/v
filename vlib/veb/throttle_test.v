// throttle_test.v - the throttle caps concurrent handlers and queues the rest.
//
// A rate limiter caps how often clients may arrive; a throttle caps how many
// may be inside at once. Without one, slow handlers park every worker thread
// on a single batch of requests. The store tests below drive explicit
// timestamps and thread rendezvous instead of sleeping through timeouts,
// except where a timeout is the behaviour under test.
module veb

import compress.gzip
import net.http
import time

struct ThrottleApp {
	Middleware[ThrottleContext]
}

struct ThrottleContext {
	Context
}

@['/work']
fn (app &ThrottleApp) work(mut ctx ThrottleContext) Result {
	return ctx.text('done')
}

@['/compressed']
fn (app &ThrottleApp) compressed(mut ctx ThrottleContext) Result {
	return ctx.text('compressible content '.repeat(100))
}

// release_after releases `slot` after `after_ms`, from its own thread, so a
// test can free a slot while another acquire is waiting on it. The store
// travels as `mut ThrottleStore` taken from a `&ThrottleStore` variable, which
// is the shape `spawn` shares rather than copies.
fn release_after(mut store ThrottleStore, slot u64, after_ms i64) {
	time.sleep(after_ms * time.millisecond)
	store.release(slot)
}

// blocking_acquire parks in `acquire` until a slot frees or `wait_ms` passes,
// so a test can hold the backlog slot while asserting about a second waiter.
fn blocking_acquire(mut store ThrottleStore, wait_ms i64) {
	store.acquire(throttle_now_ms(), wait_ms)
}

fn throttle_render(mut app ThrottleApp, method http.Method, url string, header http.Header) &Context {
	return render_request[ThrottleApp, ThrottleContext](mut app, http.Request{
		method: method
		url:    url
		header: header
	}) or { panic(err) }
}

fn throttle_get(mut app ThrottleApp, url string) &Context {
	return throttle_render(mut app, .get, url, http.Header{})
}

// refuse_unauthorized stands in for an auth middleware: it answers 401 and
// stops the chain, so neither the handler nor any after-middleware runs.
fn refuse_unauthorized(mut ctx ThrottleContext) bool {
	if (ctx.req.header.get(.authorization) or { '' }) == '' {
		ctx.res.set_status(.unauthorized)
		ctx.text('unauthorized')
		return false
	}
	return true
}

// register_throttle builds and registers a throttle the way an app's setup
// function does: the `Throttle` is a local that is gone before the first
// request arrives, so the middleware must not point at it.
fn register_throttle(mut app ThrottleApp) {
	th := throttle[ThrottleContext](max_concurrent: 1, max_waiting: 0, wait_timeout: 0)
	app.use(th.acquire())
}

// overwrite_stack reuses the stack that `register_throttle` left behind.
fn overwrite_stack() u64 {
	mut words := [64]u64{}
	for i in 0 .. words.len {
		words[i] = u64(0xdeadbeefdeadbeef)
	}
	return words[7]
}

fn test_throttle_passes_below_capacity() {
	mut store := new_throttle_store(2, 2, time.minute)
	assert store.acquire(0, 0) != 0
	assert store.acquire(0, 0) != 0
}

fn test_throttle_refuses_past_capacity_without_waiting() {
	mut store := new_throttle_store(1, 5, time.minute)
	assert store.acquire(0, 0) != 0
	// A zero wait never polls: full means refused, immediately.
	assert store.acquire(0, 0) == 0
}

fn test_a_cap_below_one_still_admits_one_request() {
	mut store := new_throttle_store(0, 0, time.minute)
	assert store.acquire(0, 0) != 0
	assert store.acquire(0, 0) == 0
}

fn test_release_frees_a_slot() {
	mut store := new_throttle_store(1, 5, time.minute)
	slot := store.acquire(0, 0)
	assert slot != 0
	assert store.acquire(0, 0) == 0
	store.release(slot)
	assert store.acquire(0, 0) != 0
}

fn test_release_of_an_unknown_slot_is_a_noop() {
	// Releasing twice, or releasing garbage, must never free another
	// request's slot: the table only deletes the exact id.
	mut store := new_throttle_store(1, 5, time.minute)
	slot := store.acquire(0, 0)
	store.release(999999)
	assert store.acquire(0, 0) == 0
	store.release(slot)
	store.release(slot)
	assert store.acquire(0, 0) != 0
}

fn test_expired_slots_are_swept() {
	mut store := new_throttle_store(1, 5, 50 * time.millisecond)
	assert store.acquire(0, 0) != 0
	assert store.acquire(0, 0) == 0
	// Past the 50ms lease the slot is gone whether or not anyone released it.
	assert store.acquire(200, 0) != 0
}

fn test_a_zero_lease_never_expires() {
	// No backlog, so the second acquire is refused at once rather than
	// waiting out a deadline on the explicit clock.
	mut store := new_throttle_store(1, 0, 0)
	assert store.acquire(0, 0) != 0
	assert store.acquire(1_000_000, 0) == 0
}

fn test_a_waiting_request_gets_a_released_slot() {
	mut store := new_throttle_store(1, 5, time.minute)
	slot := store.acquire(throttle_now_ms(), 0)
	assert slot != 0
	mut releaser := store
	t := spawn release_after(mut releaser, slot, 50)
	// Two seconds of patience for a slot that frees in fifty milliseconds.
	assert store.acquire(throttle_now_ms(), 2000) != 0
	t.wait()
}

fn test_waiting_past_the_backlog_is_refused() {
	mut store := new_throttle_store(1, 1, time.minute)
	slot := store.acquire(throttle_now_ms(), 0)
	assert slot != 0
	// Park one waiter in the single backlog slot, then confirm a second
	// waiter is refused at once instead of queueing without bound. The waiter
	// count is polled rather than slept past, so a slow machine cannot flake
	// the setup.
	mut waiter_store := store
	w := spawn blocking_acquire(mut waiter_store, 5000)
	for _ in 0 .. 200 {
		store.mu.lock()
		waiting := store.wait
		store.mu.unlock()
		if waiting == 1 {
			break
		}
		time.sleep(10 * time.millisecond)
	}
	assert store.acquire(throttle_now_ms(), 0) == 0
	store.release(slot)
	w.wait()
}

fn test_retry_after_is_whole_seconds() {
	assert retry_after_seconds(30_000) == 30
	assert retry_after_seconds(1500) == 2
	assert retry_after_seconds(300) == 1
	assert retry_after_seconds(0) == 1
}

fn test_throttle_middleware_passes_and_releases() {
	th := throttle[ThrottleContext](max_concurrent: 2, max_waiting: 2)
	mut app := ThrottleApp{}
	app.use(th.acquire())
	ctx := throttle_get(mut app, '/work')
	assert ctx.res.status_code == int(http.Status.ok)
	assert ctx.res.body == 'done'
	// veb freed the slot when the request finished.
	assert th.store.slots.len == 0
}

fn test_throttle_middleware_refuses_a_full_server() {
	th := throttle[ThrottleContext](max_concurrent: 1, max_waiting: 0)
	mut app := ThrottleApp{}
	app.use(th.acquire())
	// Occupy the only slot directly, so the render below meets a full server
	// without any threads.
	slot := th.store.acquire(throttle_now_ms(), 0)
	assert slot != 0
	ctx := throttle_get(mut app, '/work')
	assert ctx.res.status_code == int(http.Status.service_unavailable)
	assert ctx.res.body == '503 Service Unavailable'
	// Retry-After counts seconds: the default 30s wait, not 30000.
	assert (ctx.res.header.get(.retry_after) or { '' }) == '30'
	th.store.release(slot)
}

fn test_retry_after_rounds_a_short_wait_up_to_one_second() {
	th := throttle[ThrottleContext](max_concurrent: 1, max_waiting: 0, wait_timeout: 0)
	mut app := ThrottleApp{}
	app.use(th.acquire())
	slot := th.store.acquire(throttle_now_ms(), 0)
	ctx := throttle_get(mut app, '/work')
	assert ctx.res.status_code == int(http.Status.service_unavailable)
	assert (ctx.res.header.get(.retry_after) or { '' }) == '1'
	th.store.release(slot)
}

fn test_a_404_frees_its_slot() {
	// No after-middleware runs for a path nothing claims, so a slot freed only
	// there would leak: one per scanner request for /wp-admin or /.env.
	th := throttle[ThrottleContext](max_concurrent: 1, max_waiting: 0, wait_timeout: 0)
	mut app := ThrottleApp{}
	app.use(th.acquire())
	for path in ['/wp-admin', '/.env'] {
		ctx := throttle_get(mut app, path)
		assert ctx.res.status_code == int(http.Status.not_found)
		assert th.store.slots.len == 0
	}
	assert throttle_get(mut app, '/work').res.status_code == int(http.Status.ok)
}

fn test_a_request_a_later_middleware_refuses_frees_its_slot() {
	th := throttle[ThrottleContext](max_concurrent: 1, max_waiting: 0, wait_timeout: 0)
	mut app := ThrottleApp{}
	app.use(th.acquire())
	app.use(handler: refuse_unauthorized)
	for _ in 0 .. 2 {
		ctx := throttle_get(mut app, '/work')
		assert ctx.res.status_code == int(http.Status.unauthorized)
		assert th.store.slots.len == 0
	}
	ctx := throttle_render(mut app, .get, '/work', http.new_header(
		key:   .authorization
		value: 'Bearer x'
	))
	assert ctx.res.status_code == int(http.Status.ok)
	assert ctx.res.body == 'done'
}

fn test_a_request_a_route_middleware_refuses_frees_its_slot() {
	th := throttle[ThrottleContext](max_concurrent: 1, max_waiting: 0, wait_timeout: 0)
	mut app := ThrottleApp{}
	app.use(th.acquire())
	app.route_use('/work', handler: refuse_unauthorized)
	for _ in 0 .. 2 {
		ctx := throttle_get(mut app, '/work')
		assert ctx.res.status_code == int(http.Status.unauthorized)
		assert th.store.slots.len == 0
	}
}

fn test_a_cors_preflight_frees_its_slot() {
	th := throttle[ThrottleContext](max_concurrent: 1, max_waiting: 0, wait_timeout: 0)
	mut app := ThrottleApp{}
	app.use(th.acquire())
	app.use(cors[ThrottleContext](origins: ['*'], allowed_methods: [.get, .options]))
	for _ in 0 .. 2 {
		ctx := throttle_render(mut app, .options, '/work', http.new_header(
			key:   .origin
			value: 'http://example.com'
		))
		assert ctx.res.status_code == int(http.Status.ok)
		assert th.store.slots.len == 0
	}
	assert throttle_get(mut app, '/work').res.status_code == int(http.Status.ok)
}

fn test_a_compressed_response_frees_its_slot() {
	// encode_gzip returns false once it compresses, which stops the
	// after-middleware chain behind it.
	th := throttle[ThrottleContext](max_concurrent: 1, max_waiting: 0, wait_timeout: 0)
	mut app := ThrottleApp{}
	app.use(encode_gzip[ThrottleContext]())
	app.use(th.acquire())
	for _ in 0 .. 3 {
		ctx := throttle_render(mut app, .get, '/compressed', http.new_header(
			key:   .accept_encoding
			value: 'gzip'
		))
		assert ctx.res.status_code == int(http.Status.ok)
		assert (ctx.res.header.get(.content_encoding) or { '' }) == 'gzip'
		body := gzip.decompress(ctx.res.body.bytes()) or { panic(err) }
		assert body.bytestr() == 'compressible content '.repeat(100)
		assert th.store.slots.len == 0
	}
}

fn test_two_throttles_on_one_request_each_free_their_own_slot() {
	global := throttle[ThrottleContext](max_concurrent: 1, max_waiting: 0, wait_timeout: 0)
	route := throttle[ThrottleContext](max_concurrent: 1, max_waiting: 0, wait_timeout: 0)
	mut app := ThrottleApp{}
	app.use(global.acquire())
	app.route_use('/work', route.acquire())
	for _ in 0 .. 3 {
		ctx := throttle_get(mut app, '/work')
		assert ctx.res.status_code == int(http.Status.ok)
		assert global.store.slots.len == 0
		assert route.store.slots.len == 0
	}
}

fn test_a_throttle_registered_in_a_helper_keeps_working() {
	mut app := ThrottleApp{}
	register_throttle(mut app)
	assert overwrite_stack() != 0
	for _ in 0 .. 3 {
		ctx := throttle_get(mut app, '/work')
		assert ctx.res.status_code == int(http.Status.ok)
		assert ctx.res.body == 'done'
	}
}
