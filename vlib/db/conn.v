module db

import sync
import time

// Conn is a checked-out pool handle. Closing any copy invalidates all copies of the handle.
// Released handles cannot reach a reused driver.
@[heap]
pub struct Conn {
	pool       &Pool
	created_at time.Time
	state      &ConnState = &ConnState{}
}

@[heap]
struct ConnState {
mut:
	mu     &sync.Mutex = sync.new_mutex()
	driver &Driver     = unsafe { nil }
}

fn (c &Conn) ensure_active() ! {
	if isnil(c.state.driver) { return error('db: connection is released') }
}

// exec executes query on this checked-out physical connection.
pub fn (mut c Conn) exec(query string) ![]DriverRow {
	c.state.mu.lock()
	defer { c.state.mu.unlock() }
	c.ensure_active()!
	return c.state.driver.exec(query)
}

// exec_one returns one row from this checked-out physical connection.
pub fn (mut c Conn) exec_one(query string) !DriverRow {
	c.state.mu.lock()
	defer { c.state.mu.unlock() }
	c.ensure_active()!
	return c.state.driver.exec_one(query)
}

// exec_param_many executes query with parameters on this connection.
pub fn (mut c Conn) exec_param_many(query string, params []string) ![]DriverRow {
	c.state.mu.lock()
	defer { c.state.mu.unlock() }
	c.ensure_active()!
	return c.state.driver.exec_param_many(query, params)
}

// validate checks whether this checked-out connection is usable.
pub fn (mut c Conn) validate() !bool {
	c.state.mu.lock()
	defer { c.state.mu.unlock() }
	c.ensure_active()!
	return c.state.driver.validate()
}

// reset invokes the driver's backend-defined reset for this checked-out connection.
// Finish manual transactions and any required session cleanup before releasing it.
pub fn (mut c Conn) reset() ! {
	c.state.mu.lock()
	defer { c.state.mu.unlock() }
	c.ensure_active()!
	c.state.driver.reset()!
}

// close returns the connection to its pool and invalidates every copy of this handle.
pub fn (mut c Conn) close() ! {
	mut p := c.pool
	p.release(c)
}
