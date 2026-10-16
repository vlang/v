// throttle_test.v - the throttle caps concurrent handlers and queues the rest.
//
// A rate limiter caps how often clients may arrive; a throttle caps how many
// may be inside at once. Without one, slow handlers park every worker thread
// on a single batch of requests. The store tests below drive explicit
// timestamps and thread rendezvous instead of sleeping through timeouts,
// except where a timeout is the behaviour under test.
module veb

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
	store.acquire(time.now().unix_milli(), wait_ms)
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
	time.sleep(150 * time.millisecond)
	assert store.acquire(200, 0) != 0
}

fn test_a_waiting_request_gets_a_released_slot() {
	mut store := new_throttle_store(1, 5, time.minute)
	slot := store.acquire(0, 0)
	assert slot != 0
	mut releaser := store
	t := spawn release_after(mut releaser, slot, 50)
	// Two seconds of patience for a slot that frees in fifty milliseconds.
	assert store.acquire(time.now().unix_milli(), 2000) != 0
	t.wait()
}

fn test_waiting_past_the_backlog_is_refused() {
	mut store := new_throttle_store(1, 1, time.minute)
	slot := store.acquire(0, 0)
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
	assert store.acquire(time.now().unix_milli(), 0) == 0
	store.release(slot)
	w.wait()
}

fn test_throttle_middleware_passes_and_releases() {
	th := throttle[ThrottleContext](max_concurrent: 2, max_waiting: 2)
	mut app := ThrottleApp{}
	app.use(th.acquire())
	app.use(th.release())
	ctx := render_request[ThrottleApp, ThrottleContext](mut app, http.Request{
		method: .get
		url:    '/work'
	}) or { panic(err) }
	assert ctx.res.status_code == int(http.Status.ok)
	assert ctx.res.body == 'done'
	// The release half ran: no slot is still held.
	assert th.store.slots.len == 0
}

fn test_throttle_middleware_refuses_a_full_server() {
	th := throttle[ThrottleContext](max_concurrent: 1, max_waiting: 0)
	mut app := ThrottleApp{}
	app.use(th.acquire())
	app.use(th.release())
	// Occupy the only slot directly, so the render below meets a full server
	// without any threads.
	slot := th.store.acquire(time.now().unix_milli(), 0)
	assert slot != 0
	ctx := render_request[ThrottleApp, ThrottleContext](mut app, http.Request{
		method: .get
		url:    '/work'
	}) or { panic(err) }
	assert ctx.res.status_code == int(http.Status.service_unavailable)
	assert ctx.res.body == '503 Service Unavailable'
	assert (ctx.res.header.get_custom('Retry-After') or { '' }) != ''
	th.store.release(slot)
}
