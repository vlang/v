module veb

import sync
import time

// ThrottleOptions configures the `throttle` middleware: at most
// `max_concurrent` requests run their handlers at once, and at most
// `max_waiting` further requests queue for a slot, waiting up to
// `wait_timeout` each. Past either bound the middleware answers 503 with a
// `Retry-After` header instead of letting the pileup reach the handlers.
//
// A rate limiter caps how often clients may arrive; a throttle caps how many
// may be inside at once.
//
// A queued request waits by sleeping on the thread that serves it. veb's
// fasthttp backend serves each connection inline on a fixed set of worker
// threads, so a waiting request also stalls the other connections of its
// thread, and no more requests than there are worker threads run handlers at
// once. Keep `max_concurrent` below the worker count for the cap to bind, and
// set `max_waiting: 0` to refuse at once instead of queueing.
//
// veb frees a slot when its request finishes, however it finishes.
// `lease_timeout` is only a backstop: a slot held longer is reclaimed, so a
// handler still running past it over-admits rather than blocks everyone. Set
// it above your slowest handler, or to 0 to never reclaim a slot early.
@[params]
pub struct ThrottleOptions {
pub:
	max_concurrent int           = 100
	max_waiting    int           = 100
	wait_timeout   time.Duration = 30 * time.second
	lease_timeout  time.Duration = time.minute
	message        string        = '503 Service Unavailable'
}

// ThrottleStore counts handler slots behind a mutex: veb serves on worker
// threads, so two requests race here on every arrival. Slots are ids mapped
// to expiry timestamps; sweeping the expired ones bounds what a stuck
// handler can hold.
struct ThrottleStore {
	max_concurrent int
	max_waiting    int
	lease_ms       i64
mut:
	mu    &sync.Mutex
	slots map[u64]i64
	wait  int
	next  u64
}

fn new_throttle_store(max_concurrent int, max_waiting int, lease time.Duration) &ThrottleStore {
	return &ThrottleStore{
		// A cap below one would refuse every request.
		max_concurrent: if max_concurrent < 1 { 1 } else { max_concurrent }
		max_waiting:    max_waiting
		lease_ms:       i64(lease) / i64(time.millisecond)
		mu:             sync.new_mutex()
	}
}

// throttle_now_ms reads a monotonic clock in milliseconds, so stepping the
// wall clock neither stretches nor cuts short a lease or a wait.
fn throttle_now_ms() i64 {
	return i64(time.sys_mono_now() / 1_000_000)
}

// acquire takes a handler slot, waiting up to `wait_ms` for one, and returns
// the slot id with it. It returns 0 when no slot can start promptly: the
// waiting queue is full, or nothing frees before the timeout. `now_ms` is a
// `throttle_now_ms` reading.
fn (mut s ThrottleStore) acquire(now_ms i64, wait_ms i64) u64 {
	deadline := now_ms + wait_ms
	s.mu.lock()
	s.sweep_expired(now_ms)
	if s.live_slots() < s.max_concurrent {
		id := s.take_slot(now_ms)
		s.mu.unlock()
		return id
	}
	if s.wait >= s.max_waiting {
		s.mu.unlock()
		return 0
	}
	s.wait++
	s.mu.unlock()
	for throttle_now_ms() < deadline {
		time.sleep(5 * time.millisecond)
		s.mu.lock()
		now := throttle_now_ms()
		s.sweep_expired(now)
		if s.live_slots() < s.max_concurrent {
			id := s.take_slot(now)
			s.wait--
			s.mu.unlock()
			return id
		}
		s.mu.unlock()
	}
	s.mu.lock()
	s.wait--
	s.mu.unlock()
	return 0
}

// release frees the slot `acquire` returned. An unknown or already-swept id
// is a no-op, so releasing twice never frees another request's slot.
fn (mut s ThrottleStore) release(slot u64) {
	if slot == 0 {
		return
	}
	s.mu.lock()
	s.slots.delete(slot)
	s.mu.unlock()
}

// live_slots counts slots whose lease has not expired.
fn (s &ThrottleStore) live_slots() int {
	return s.slots.len
}

// take_slot records a fresh slot expiring `lease_ms` from `now_ms`. The
// caller holds the mutex and has checked capacity.
fn (mut s ThrottleStore) take_slot(now_ms i64) u64 {
	s.next++
	s.slots[s.next] = now_ms + s.lease_ms
	return s.next
}

// sweep_expired drops slots whose lease passed. A lease of 0 or less never
// expires. The caller holds the mutex.
fn (mut s ThrottleStore) sweep_expired(now_ms i64) {
	if s.lease_ms <= 0 {
		return
	}
	for id, expiry in s.slots {
		if expiry <= now_ms {
			s.slots.delete(id)
		}
	}
}

// ThrottleLease is one handler slot a throttle took for a request. The
// request context keeps its leases, so veb can free each slot in the store it
// came from, even when several throttles apply to one request.
struct ThrottleLease {
	store &ThrottleStore
	slot  u64
}

// release_throttle_leases frees every slot a throttle took for this request.
// `handle_route` defers it, so it runs however the request ends: after the
// handler and the after-middleware, but also on a 404, or when a later
// middleware refuses the request, where no after-middleware runs.
fn (mut ctx Context) release_throttle_leases() {
	for lease in ctx.throttle_leases {
		mut store := lease.store
		store.release(lease.slot)
	}
	ctx.throttle_leases = []
}

// retry_after_seconds turns a wait timeout into a `Retry-After` value, which
// RFC 9110 counts in whole seconds: rounded up, and never 0.
fn retry_after_seconds(wait_ms i64) i64 {
	secs := (wait_ms + 999) / 1000
	return if secs < 1 { 1 } else { secs }
}

// Throttle is one cap on concurrent handlers: a slot store and the middleware
// that takes slots from it. Every registration of the same throttle's
// middleware shares the one cap.
pub struct Throttle[T] {
mut:
	store   &ThrottleStore
	options ThrottleOptions
	wait_ms i64
}

// throttle builds a throttle. Register its middleware globally or on routes,
// after any middleware that refuses requests cheaply, such as authentication,
// so refused requests never wait for a slot:
// ```v
// th := veb.throttle[Context](max_concurrent: 8)
// app.use(th.acquire())
// ```
// There is no release half to register: veb frees the slot when the request
// finishes, whichever way it finishes.
pub fn throttle[T](options ThrottleOptions) Throttle[T] {
	return Throttle[T]{
		store:   new_throttle_store(options.max_concurrent, options.max_waiting,
			options.lease_timeout)
		options: options
		wait_ms: i64(options.wait_timeout) / i64(time.millisecond)
	}
}

// acquire returns the middleware that takes a handler slot before the
// handler runs. A request that cannot start promptly is refused with 503 and
// `Retry-After` instead. The middleware keeps its own copy of the throttle,
// so the `Throttle` may be a local of a setup function that returns before
// the first request arrives.
pub fn (t Throttle[T]) acquire() MiddlewareOptions[T] {
	retry_after := retry_after_seconds(t.wait_ms).str()
	return MiddlewareOptions[T]{
		handler: fn [t, retry_after] [T](mut ctx T) bool {
			slot := t.store.acquire(throttle_now_ms(), t.wait_ms)
			if slot != 0 {
				ctx.Context.throttle_leases << ThrottleLease{
					store: t.store
					slot:  slot
				}
				return true
			}
			ctx.res.set_status(.service_unavailable)
			ctx.res.header.set(.retry_after, retry_after)
			ctx.text(t.options.message)
			return false
		}
	}
}
