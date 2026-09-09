module pg

import sync
import time

// PoolStats reports the current state of a `DB`'s connection pool.
pub struct PoolStats {
pub:
	max_open_connections int // configured limit (0 = unlimited)
	open_connections     int // total in-use + idle conns
	in_use               int // conns currently checked out
	idle                 int // conns parked as idle
	wait_count           int // number of callers currently blocked on acquire
}

// PoolConfig configures pool behavior at construction time.
@[params]
pub struct PoolConfig {
pub:
	max_open_conns    int // 0 = unlimited
	max_idle_conns    int = 2 // 0 = keep no idle conns
	conn_max_lifetime time.Duration // 0 = unlimited
}

// IdleSlot is the pool's internal representation of a pooled libpq handle.
// The pool tracks raw PGconn* metadata in idle/waiter channels; `acquire`
// allocates a fresh `&Conn` wrapping the slot and `release` extracts the
// metadata back. Keeping the wrapper separate from the slot means a stale
// `&Conn` kept by user code after `close()` cannot be revived even when
// the pool re-hands the same physical PGconn* to another caller.
struct IdleSlot {
	handle     voidptr
	created_at time.Time
mut:
	bad bool
}

@[heap]
struct Pool {
mut:
	mu           &sync.Mutex = unsafe { nil }
	conninfo     string
	max_open     int
	max_idle     int
	max_lifetime time.Duration
	idle         []IdleSlot
	open_count   int
	waiters      []chan IdleSlot
	closed       bool
	// last_ids stores the per-thread last-inserted id so DB.last_id() returns
	// the value captured on the same pooled conn that ran the INSERT, instead
	// of calling LASTVAL() on whatever conn the pool happens to hand out next
	// (which is session-scoped and would return 0 or a wrong value).
	last_ids_mu &sync.Mutex = unsafe { nil }
	last_ids    map[u64]i64
}

fn new_pool(conninfo string, cfg PoolConfig) &Pool {
	mut p := &Pool{
		mu:           sync.new_mutex()
		last_ids_mu:  sync.new_mutex()
		conninfo:     conninfo
		max_open:     cfg.max_open_conns
		max_idle:     cfg.max_idle_conns
		max_lifetime: cfg.conn_max_lifetime
	}
	if p.max_open < 0 {
		p.max_open = 0
	}
	if p.max_idle < 0 {
		p.max_idle = 0
	}
	return p
}

// wrap_slot builds the fresh `&Conn` that gets handed to a caller. Each
// acquire allocates a new wrapper so a previously-checked-out (and since
// released) `&Conn` reference can never be reused to reach the underlying
// PGconn*.
fn (p &Pool) wrap_slot(slot IdleSlot) &Conn {
	return &Conn{
		conn:       slot.handle
		pool:       unsafe { p }
		created_at: slot.created_at
		bad:        slot.bad
	}
}

fn (mut p Pool) acquire() !&Conn {
	for {
		p.mu.lock()
		if p.closed {
			p.mu.unlock()
			return error('pg: pool is closed')
		}
		// Pop newest idle slot (LIFO keeps the freshest connection warm)
		for p.idle.len > 0 {
			slot := p.idle.last()
			p.idle.delete_last()
			if slot_expired(slot, p.max_lifetime) || slot_bad(slot) {
				physical_close_handle(slot.handle)
				p.open_count--
				continue
			}
			p.mu.unlock()
			return p.wrap_slot(slot)
		}
		// Capacity available: open a new conn outside the lock
		if p.max_open == 0 || p.open_count < p.max_open {
			p.open_count++
			conninfo := p.conninfo
			p.mu.unlock()
			slot := connect_slot(conninfo) or {
				p.mu.lock()
				p.open_count--
				p.mu.unlock()
				return err
			}
			// close() may have run while we were dialing outside the lock.
			// Honor the close contract by tearing the fresh handle down
			// instead of returning a live Conn after shutdown.
			p.mu.lock()
			if p.closed {
				p.open_count--
				p.mu.unlock()
				physical_close_handle(slot.handle)
				return error('pg: pool is closed')
			}
			p.mu.unlock()
			return p.wrap_slot(slot)
		}
		// At capacity: wait for a release/close/cap-raise to signal us.
		// Senders transfer slot ownership via this channel; a sentinel slot
		// with a nil handle means "capacity changed, retry the acquire
		// loop" (used by set_max_open when raising the limit).
		waiter := chan IdleSlot{cap: 1}
		p.waiters << waiter
		p.mu.unlock()
		slot := <-waiter or { return error('pg: pool was closed while waiting for connection') }
		if isnil(slot.handle) {
			continue
		}
		return p.wrap_slot(slot)
	}
	return error('pg: unreachable')
}

fn (mut p Pool) release(conn &Conn) {
	if isnil(conn) {
		return
	}
	mut c := unsafe { conn }
	// Detach the wrapper from the physical handle before doing anything else.
	// This is what makes a stale `&Conn` reference held by user code inert:
	// any subsequent method call on it will see `c.conn == nil` and error.
	// It is also the idempotency guard against double-close — a second
	// release() finds nothing to return to the pool and is a no-op.
	if isnil(c.conn) {
		return
	}
	slot := IdleSlot{
		handle:     c.conn
		created_at: c.created_at
		bad:        c.bad
	}
	c.conn = unsafe { nil }
	c.pool = unsafe { nil }
	p.mu.lock()
	if p.closed {
		// close() only decremented open_count for slots it found parked;
		// in-use conns are accounted for here as they trickle back.
		p.open_count--
		p.mu.unlock()
		physical_close_handle(slot.handle)
		return
	}
	// Discard broken or expired conns
	if slot_bad(slot) || slot_expired(slot, p.max_lifetime) {
		physical_close_handle(slot.handle)
		p.open_count--
		// A capacity slot just opened; hand it to a waiter as a fresh conn
		if p.waiters.len > 0 && (p.max_open == 0 || p.open_count < p.max_open) {
			waiter := p.waiters[0]
			p.waiters.delete(0)
			p.open_count++
			conninfo := p.conninfo
			p.mu.unlock()
			new_slot := connect_slot(conninfo) or {
				p.mu.lock()
				p.open_count--
				// Capacity is now open but the dial just failed. Wake every
				// other parked waiter too, otherwise they hang forever waiting
				// for a release that will never come (e.g. max_open=1).
				extras := p.waiters.clone()
				p.waiters = []chan IdleSlot{}
				p.mu.unlock()
				waiter.close()
				for w in extras {
					w.close()
				}
				return
			}
			p.mu.lock()
			if p.closed {
				// Pool closed during the dial: drop the new conn and signal the waiter.
				p.open_count--
				p.mu.unlock()
				physical_close_handle(new_slot.handle)
				waiter.close()
				return
			}
			waiter <- new_slot
			p.mu.unlock()
			return
		}
		p.mu.unlock()
		return
	}
	// If a recent set_max_open() shrank the cap below open_count, this
	// returning slot has to be retired even when a waiter is queued —
	// otherwise the fast hand-off keeps open_count pinned above the new
	// limit forever under steady traffic. Wake the waiter with a retry
	// sentinel so it re-evaluates capacity under the new cap.
	if p.max_open > 0 && p.open_count > p.max_open {
		physical_close_handle(slot.handle)
		p.open_count--
		if p.waiters.len > 0 {
			waiter := p.waiters[0]
			p.waiters.delete(0)
			waiter <- IdleSlot{
				handle: unsafe { nil }
			}
		}
		p.mu.unlock()
		return
	}
	// Healthy conn: prefer handing it directly to a waiter. Send under the
	// lock (cap:1 makes it non-blocking) so close() can't slip in and orphan
	// the popped waiter with a live conn.
	if p.waiters.len > 0 {
		waiter := p.waiters[0]
		p.waiters.delete(0)
		waiter <- slot
		p.mu.unlock()
		return
	}
	// Park as idle, unless we'd exceed max_idle (0 = keep no idle conns)
	// or we are over a recently-shrunk max_open and need to converge.
	if p.idle.len >= p.max_idle || (p.max_open > 0 && p.open_count > p.max_open) {
		physical_close_handle(slot.handle)
		p.open_count--
		p.mu.unlock()
		return
	}
	p.idle << slot
	p.mu.unlock()
}

fn (mut p Pool) close() {
	p.mu.lock()
	if p.closed {
		p.mu.unlock()
		return
	}
	p.closed = true
	for slot in p.idle {
		physical_close_handle(slot.handle)
	}
	idle_len := p.idle.len
	p.idle = []IdleSlot{}
	p.open_count -= idle_len
	for waiter in p.waiters {
		waiter.close()
	}
	p.waiters = []chan IdleSlot{}
	p.mu.unlock()
}

fn (mut p Pool) stats() PoolStats {
	p.mu.lock()
	stats := PoolStats{
		max_open_connections: p.max_open
		open_connections:     p.open_count
		idle:                 p.idle.len
		in_use:               p.open_count - p.idle.len
		wait_count:           p.waiters.len
	}
	p.mu.unlock()
	return stats
}

fn (mut p Pool) set_max_open(n int) {
	mut nn := n
	if nn < 0 {
		nn = 0
	}
	p.mu.lock()
	p.max_open = nn
	// Shrink: drop idle slots until we are back under the new cap. Without
	// this, acquire() pops idle slots before checking max_open, so two
	// callers could still both succeed after set_max_open_conns(1) when two
	// warm conns are parked. In-use conns can't be reclaimed here — they get
	// discarded on release once open_count is over the cap.
	if nn > 0 {
		for p.idle.len > 0 && p.open_count > nn {
			slot := p.idle.last()
			p.idle.delete_last()
			physical_close_handle(slot.handle)
			p.open_count--
		}
	}
	// Raise: wake parked waiters so they can retry the acquire loop and
	// dial against the new headroom. Without this nudge they stay blocked
	// until some other release happens, which may never come if all current
	// conns are long-lived.
	if p.waiters.len > 0 && (nn == 0 || p.open_count < nn) {
		spare := if nn == 0 { p.waiters.len } else { nn - p.open_count }
		mut n_wake := if spare < p.waiters.len { spare } else { p.waiters.len }
		for n_wake > 0 {
			waiter := p.waiters[0]
			p.waiters.delete(0)
			waiter <- IdleSlot{
				handle: unsafe { nil }
			}
			n_wake--
		}
	}
	p.mu.unlock()
}

fn (mut p Pool) set_max_idle(n int) {
	mut nn := n
	if nn < 0 {
		nn = 0
	}
	p.mu.lock()
	p.max_idle = nn
	for p.idle.len > nn {
		slot := p.idle.last()
		p.idle.delete_last()
		physical_close_handle(slot.handle)
		p.open_count--
	}
	p.mu.unlock()
}

fn (mut p Pool) set_conn_max_lifetime(d time.Duration) {
	p.mu.lock()
	p.max_lifetime = d
	p.mu.unlock()
}

// stash_last_id records `id` as the last-inserted id for the current thread.
// Called by `DB.insert` right after running INSERT on the pinned conn, so the
// next `DB.last_id()` call on the same thread returns this exact value rather
// than running LASTVAL() on a different pooled session.
fn (mut p Pool) stash_last_id(id int) {
	tid := sync.thread_id()
	p.last_ids_mu.lock()
	p.last_ids[tid] = i64(id)
	p.last_ids_mu.unlock()
}

// take_last_id returns the last-inserted id stashed by this thread's most
// recent `DB.insert`, or 0 if there is none. Consuming on read keeps the
// map bounded as threads come and go, and prevents a future thread that
// reuses this tid from observing a stale id before doing its own insert.
fn (mut p Pool) take_last_id() int {
	tid := sync.thread_id()
	p.last_ids_mu.lock()
	id := p.last_ids[tid] or { i64(0) }
	p.last_ids.delete(tid)
	p.last_ids_mu.unlock()
	return int(id)
}
