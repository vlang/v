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
// may be inside at once. Slow handlers and slow clients need the second one:
// without it every worker thread can end up parked on one batch of requests.
//
// A slot held past `lease_timeout` is reclaimed, so a request that takes one
// and never releases it — a path nothing claims, which answers 404 before any
// after-middleware runs — degrades instead of leaking forever. Set the lease
// above your slowest handler: a handler still running past it over-admits
// rather than fails.
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
// to expiry timestamps; sweeping the expired ones bounds what vanished
// requests can hold.
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
		max_concurrent: max_concurrent
		max_waiting:    max_waiting
		lease_ms:       i64(lease) / i64(time.millisecond)
		mu:             sync.new_mutex()
	}
}

// acquire takes a handler slot, waiting up to `wait_ms` for one, and returns
// the slot id with it. It returns 0 when no slot can start promptly: the
// waiting queue is full, or nothing frees before the timeout.
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
	for time.now().unix_milli() < deadline {
		time.sleep(5 * time.millisecond)
		s.mu.lock()
		s.sweep_expired(time.now().unix_milli())
		if s.live_slots() < s.max_concurrent {
			id := s.take_slot(time.now().unix_milli())
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

// sweep_expired drops slots whose lease passed. The caller holds the mutex.
fn (mut s ThrottleStore) sweep_expired(now_ms i64) {
	for id, expiry in s.slots {
		if expiry <= now_ms {
			s.slots.delete(id)
		}
	}
}

// Throttle is the acquire half and the release half of one throttle sharing
// one store. veb middleware runs before a handler or after it, never around
// it, so the two halves register separately: the slot id travels on the
// request context between them.
pub struct Throttle[T] {
mut:
	store   &ThrottleStore
	options ThrottleOptions
	wait_ms i64
}

// throttle builds the pair. Register both halves, globally or on the same
// route:
// ```v
// th := veb.throttle[Context]()
// app.use(th.acquire())
// app.use(th.release())
// ```
pub fn throttle[T](options ThrottleOptions) Throttle[T] {
	return Throttle[T]{
		store:   new_throttle_store(options.max_concurrent, options.max_waiting,
			options.lease_timeout)
		options: options
		wait_ms: i64(options.wait_timeout) / i64(time.millisecond)
	}
}

// acquire takes a handler slot before the handler runs and records it on the
// request context. A request that cannot start promptly is refused with 503
// and `Retry-After` instead.
pub fn (t &Throttle[T]) acquire() MiddlewareOptions[T] {
	return MiddlewareOptions[T]{
		handler: fn [t] [T](mut ctx T) bool {
			slot := t.store.acquire(time.now().unix_milli(), t.wait_ms)
			if slot != 0 {
				ctx.Context.throttle_slot = slot
				return true
			}
			ctx.res.set_status(.service_unavailable)
			ctx.res.header.set_custom('Retry-After', t.wait_ms.str()) or {}
			ctx.text(t.options.message)
			return false
		}
	}
}

// release frees the slot `acquire` took, after the handler ran. Requests the
// acquire half refused, and paths nothing claimed, never reach it.
pub fn (t &Throttle[T]) release() MiddlewareOptions[T] {
	return MiddlewareOptions[T]{
		after:   true
		handler: fn [t] [T](mut ctx T) bool {
			t.store.release(ctx.Context.throttle_slot)
			ctx.Context.throttle_slot = 0
			return true
		}
	}
}
