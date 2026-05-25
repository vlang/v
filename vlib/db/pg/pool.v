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
}

fn new_pool(conninfo string, cfg PoolConfig) &Pool {
	mut p := &Pool{
		mu:           sync.new_mutex()
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
			return conn
		}
		// At capacity: wait for a release/close to signal us
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
				p.mu.unlock()
				waiter.close()
				return
			}
			waiter <- new_conn
			return
		}
		p.mu.unlock()
		return
	}
	// Healthy conn: prefer handing it directly to a waiter
	if p.waiters.len > 0 {
		waiter := p.waiters[0]
		p.waiters.delete(0)
		p.mu.unlock()
		waiter <- c
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
