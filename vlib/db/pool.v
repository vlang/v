module db

import sync
import time

// DriverFactory opens independent physical connections for a Pool.
pub interface DriverFactory {
	connect() !&Driver
}

// PoolConfig sets connection limits. Zero max_open_conns means unlimited.
@[params]
pub struct PoolConfig {
pub:
	max_open_conns    int
	max_idle_conns    int = 2
	conn_max_lifetime time.Duration
}

// PoolStats describes physical connections and callers waiting for capacity.
pub struct PoolStats {
pub:
	max_open_connections int
	open_connections     int
	in_use               int
	idle                 int
	wait_count           int
}

struct PoolSlot {
	driver     &Driver = unsafe { nil }
	created_at time.Time
}

// Pool manages independent Driver connections supplied by a DriverFactory.
@[heap]
pub struct Pool {
	factory DriverFactory
mut:
	mu           &sync.Mutex = sync.new_mutex()
	idle         []PoolSlot
	waiters      []chan PoolSlot
	open_count   int
	max_open     int
	max_idle     int
	max_lifetime time.Duration
	closed       bool
}

// new_pool creates a pool without opening a physical connection.
pub fn new_pool(factory DriverFactory, cfg PoolConfig) &Pool {
	return &Pool{
		factory:      factory
		max_open:     if cfg.max_open_conns < 0 { 0 } else { cfg.max_open_conns }
		max_idle:     if cfg.max_idle_conns < 0 { 0 } else { cfg.max_idle_conns }
		max_lifetime: cfg.conn_max_lifetime
	}
}

fn pool_slot_expired(slot PoolSlot, lifetime time.Duration) bool {
	return lifetime > 0 && time.since(slot.created_at) >= lifetime
}

fn pool_close_slot(slot PoolSlot) {
	mut driver := slot.driver
	driver.close() or {}
}

// wake_waiters must be called with mu locked. A nil slot means retry acquisition.
fn (mut p Pool) wake_waiters() {
	for waiter in p.waiters {
		waiter <- PoolSlot{}
	}
	p.waiters = []chan PoolSlot{}
}

fn (mut p Pool) discard(slot PoolSlot) {
	pool_close_slot(slot)
	p.mu.lock()
	p.open_count--
	p.wake_waiters()
	p.mu.unlock()
}

fn (p &Pool) wrap(slot PoolSlot) &Conn {
	return &Conn{
		state:      &ConnState{ driver: slot.driver }
		pool:       unsafe { p }
		created_at: slot.created_at
	}
}

// acquire returns a fresh handle, waiting when the physical connection limit is reached.
// Reused connections are validated before they are handed to the caller.
pub fn (mut p Pool) acquire() !&Conn {
	for {
		p.mu.lock()
		if p.closed {
			p.mu.unlock()
			return error('db: pool is closed')
		}
		if p.idle.len > 0 {
			slot := p.idle.pop()
			lifetime := p.max_lifetime
			p.mu.unlock()
			mut driver := slot.driver
			valid := driver.validate() or { false }
			if !valid || pool_slot_expired(slot, lifetime) {
				p.discard(slot)
				continue
			}
			p.mu.lock()
			closed := p.closed
			p.mu.unlock()
			if closed {
				p.discard(slot)
				return error('db: pool is closed')
			}
			return p.wrap(slot)
		}
		if p.max_open == 0 || p.open_count < p.max_open {
			p.open_count++
			p.mu.unlock()
			driver := p.factory.connect() or {
				p.mu.lock()
				p.open_count--
				p.wake_waiters()
				p.mu.unlock()
				return err
			}
			if isnil(driver) {
				p.mu.lock()
				p.open_count--
				p.wake_waiters()
				p.mu.unlock()
				return error('db: driver factory returned a nil connection')
			}
			slot := PoolSlot{ driver: driver, created_at: time.now() }
			p.mu.lock()
			closed := p.closed
			p.mu.unlock()
			if closed {
				p.discard(slot)
				return error('db: pool is closed')
			}
			return p.wrap(slot)
		}
		waiter := chan PoolSlot{cap: 1}
		p.waiters << waiter
		p.mu.unlock()
		slot := <-waiter or { return error('db: pool is closed') }
		if !isnil(slot.driver) {
			mut driver := slot.driver
			valid := driver.validate() or { false }
			p.mu.lock()
			closed := p.closed
			lifetime := p.max_lifetime
			p.mu.unlock()
			if !valid || closed || pool_slot_expired(slot, lifetime) {
				p.discard(slot)
				if closed { return error('db: pool is closed') }
				continue
			}
			return p.wrap(slot)
		}
	}
	return error('db: unreachable')
}

// release detaches the handle, resets its session, and returns or closes the connection.
// Releasing a handle again is harmless; handles belonging to another pool are ignored.
pub fn (mut p Pool) release(conn &Conn) {
	if isnil(conn) {
		return
	}
	mut state := conn.state
	state.mu.lock()
	if isnil(state.driver) || conn.pool != p {
		state.mu.unlock()
		return
	}
	slot := PoolSlot{ driver: state.driver, created_at: conn.created_at }
	state.driver = unsafe { nil }
	state.mu.unlock()
	mut driver := slot.driver
	driver.reset() or {
		p.discard(slot)
		return
	}
	p.mu.lock()
	if p.closed || pool_slot_expired(slot, p.max_lifetime)
		|| (p.max_open > 0 && p.open_count > p.max_open) {
		p.mu.unlock()
		p.discard(slot)
		return
	}
	if p.waiters.len > 0 {
		waiter := p.waiters[0]
		p.waiters.delete(0)
		waiter <- slot
		p.mu.unlock()
		return
	}
	if p.idle.len >= p.max_idle {
		p.mu.unlock()
		p.discard(slot)
		return
	}
	p.idle << slot
	p.mu.unlock()
}

// close stops acquisition, wakes waiting callers, and closes idle connections.
// Checked-out connections close when released; close is idempotent.
pub fn (mut p Pool) close() {
	p.mu.lock()
	if p.closed {
		p.mu.unlock()
		return
	}
	p.closed = true
	slots := p.idle.clone()
	p.open_count -= slots.len
	p.idle = []PoolSlot{}
	for waiter in p.waiters { waiter.close() }
	p.waiters = []chan PoolSlot{}
	p.mu.unlock()
	for slot in slots { pool_close_slot(slot) }
}

// stats returns a consistent snapshot of the pool's counters.
pub fn (mut p Pool) stats() PoolStats {
	p.mu.lock()
	defer { p.mu.unlock() }
	return PoolStats{
		max_open_connections: p.max_open
		open_connections:     p.open_count
		in_use:               p.open_count - p.idle.len
		idle:                 p.idle.len
		wait_count:           p.waiters.len
	}
}

// set_max_open changes capacity; negative values mean unlimited.
pub fn (mut p Pool) set_max_open(n int) {
	p.mu.lock()
	p.max_open = if n < 0 { 0 } else { n }
	mut excess := []PoolSlot{}
	for p.max_open > 0 && p.open_count > p.max_open && p.idle.len > 0 {
		excess << p.idle.pop()
		p.open_count--
	}
	p.wake_waiters()
	p.mu.unlock()
	for slot in excess { pool_close_slot(slot) }
}

// set_max_idle changes the idle connection limit; zero retains no idle connections.
pub fn (mut p Pool) set_max_idle(n int) {
	p.mu.lock()
	p.max_idle = if n < 0 { 0 } else { n }
	mut excess := []PoolSlot{}
	for p.idle.len > p.max_idle {
		excess << p.idle.pop()
		p.open_count--
	}
	p.mu.unlock()
	for slot in excess { pool_close_slot(slot) }
}

// set_max_lifetime changes connection lifetime; nonpositive values disable expiration.
pub fn (mut p Pool) set_max_lifetime(d time.Duration) {
	p.mu.lock()
	p.max_lifetime = d
	p.mu.unlock()
}
