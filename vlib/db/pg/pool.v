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

@[heap]
struct Pool {
mut:
	mu           &sync.Mutex = unsafe { nil }
	conninfo     string
	max_open     int
	max_idle     int
	max_lifetime time.Duration
	idle         []&Conn
	open_count   int
	waiters      []chan &Conn
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

fn (mut p Pool) acquire() !&Conn {
	for {
		p.mu.lock()
		if p.closed {
			p.mu.unlock()
			return error('pg: pool is closed')
		}
		// Pop newest idle conn (LIFO keeps the freshest connection warm)
		for p.idle.len > 0 {
			conn := p.idle.last()
			p.idle.delete_last()
			if conn.is_expired(p.max_lifetime) || conn.is_bad() {
				unsafe { conn.physical_close() }
				p.open_count--
				continue
			}
			mut mc := unsafe { conn }
			mc.checked_out = true
			p.mu.unlock()
			return conn
		}
		// Capacity available: open a new conn outside the lock
		if p.max_open == 0 || p.open_count < p.max_open {
			p.open_count++
			conninfo := p.conninfo
			p.mu.unlock()
			conn := connect_conn(conninfo, unsafe { p }) or {
				p.mu.lock()
				p.open_count--
				p.mu.unlock()
				return err
			}
			mut mc := unsafe { conn }
			mc.checked_out = true
			return conn
		}
		// At capacity: wait for a release/close to signal us. Senders flip
		// checked_out under the lock before transferring ownership.
		waiter := chan &Conn{cap: 1}
		p.waiters << waiter
		p.mu.unlock()
		conn := <-waiter or { return error('pg: pool was closed while waiting for connection') }
		return conn
	}
	return error('pg: unreachable')
}

fn (mut p Pool) release(conn &Conn) {
	if isnil(conn) {
		return
	}
	mut c := unsafe { conn }
	p.mu.lock()
	// Idempotency guard: a second close() on the same handle (manual + deferred
	// close, double-defer, etc.) would otherwise enqueue the conn twice and let
	// two callers receive the same PGconn*, violating libpq's single-thread
	// usage rule.
	if !c.checked_out {
		p.mu.unlock()
		return
	}
	c.checked_out = false
	if p.closed {
		p.mu.unlock()
		unsafe { c.physical_close() }
		return
	}
	// Discard broken or expired conns
	if c.is_bad() || c.is_expired(p.max_lifetime) {
		unsafe { c.physical_close() }
		p.open_count--
		// A capacity slot just opened; hand it to a waiter as a fresh conn
		if p.waiters.len > 0 && (p.max_open == 0 || p.open_count < p.max_open) {
			waiter := p.waiters[0]
			p.waiters.delete(0)
			p.open_count++
			conninfo := p.conninfo
			p.mu.unlock()
			new_conn := connect_conn(conninfo, unsafe { p }) or {
				p.mu.lock()
				p.open_count--
				// Capacity is now open but the dial just failed. Wake every
				// other parked waiter too, otherwise they hang forever waiting
				// for a release that will never come (e.g. max_open=1).
				extras := p.waiters.clone()
				p.waiters = []chan &Conn{}
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
				unsafe { new_conn.physical_close() }
				waiter.close()
				return
			}
			mut mnc := unsafe { new_conn }
			mnc.checked_out = true
			waiter <- new_conn
			p.mu.unlock()
			return
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
		c.checked_out = true
		waiter <- c
		p.mu.unlock()
		return
	}
	// Park as idle, unless we'd exceed max_idle (0 = keep no idle conns)
	if p.idle.len >= p.max_idle {
		unsafe { c.physical_close() }
		p.open_count--
		p.mu.unlock()
		return
	}
	p.idle << c
	p.mu.unlock()
}

fn (mut p Pool) close() {
	p.mu.lock()
	if p.closed {
		p.mu.unlock()
		return
	}
	p.closed = true
	for conn in p.idle {
		unsafe { conn.physical_close() }
	}
	idle_len := p.idle.len
	p.idle = []&Conn{}
	p.open_count -= idle_len
	for waiter in p.waiters {
		waiter.close()
	}
	p.waiters = []chan &Conn{}
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
	// If shrinking below open_count, idle conns get pruned as they come back.
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
		conn := p.idle.last()
		p.idle.delete_last()
		unsafe { conn.physical_close() }
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
// recent `DB.insert`, or 0 if there is none.
fn (mut p Pool) take_last_id() int {
	tid := sync.thread_id()
	p.last_ids_mu.lock()
	id := p.last_ids[tid] or { i64(0) }
	p.last_ids_mu.unlock()
	return int(id)
}
