module db

import sync
import time

// Conn is a checked-out pool handle. Released handles cannot reach a reused driver.
@[heap]
pub struct Conn {
	pool       &Pool
	created_at time.Time
mut:
	mu     &sync.Mutex = sync.new_mutex()
	driver &Driver     = unsafe { nil }
}

fn (c &Conn) ensure_active() ! {
	if isnil(c.driver) { return error('db: connection is released') }
}

// exec executes query on this checked-out physical connection.
pub fn (mut c Conn) exec(query string) ![]DriverRow {
	c.mu.lock()
	defer { c.mu.unlock() }
	c.ensure_active()!
	return c.driver.exec(query)
}

// exec_one returns one row from this checked-out physical connection.
pub fn (mut c Conn) exec_one(query string) !DriverRow {
	c.mu.lock()
	defer { c.mu.unlock() }
	c.ensure_active()!
	return c.driver.exec_one(query)
}

// exec_param_many executes query with parameters on this connection.
pub fn (mut c Conn) exec_param_many(query string, params []string) ![]DriverRow {
	c.mu.lock()
	defer { c.mu.unlock() }
	c.ensure_active()!
	return c.driver.exec_param_many(query, params)
}

// validate checks whether this checked-out connection is usable.
pub fn (mut c Conn) validate() !bool {
	c.mu.lock()
	defer { c.mu.unlock() }
	c.ensure_active()!
	return c.driver.validate()
}

// reset invokes the driver's backend-defined reset for this checked-out connection.
// Finish manual transactions and any required session cleanup before releasing it.
pub fn (mut c Conn) reset() ! {
	c.mu.lock()
	defer { c.mu.unlock() }
	c.ensure_active()!
	c.driver.reset()!
}

// close returns the connection to its pool and invalidates this handle.
pub fn (mut c Conn) close() ! {
	mut p := c.pool
	p.release(c)
}
