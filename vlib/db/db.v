module db

import time

struct ConfigDriverFactory {
	config DriverConfig
}

fn (factory ConfigDriverFactory) connect() !&Driver {
	return open(factory.config)
}

// DB is a pool-backed SQL handle. Each operation borrows and releases one connection.
@[heap]
pub struct DB {
mut:
	pool &Pool
}

// open_pooled constructs a lazily connected pool for a built-in database driver.
// Connection errors are reported when a connection is first acquired.
pub fn open_pooled(config DriverConfig, cfg PoolConfig) &DB {
	return &DB{ pool: new_pool(ConfigDriverFactory{ config: config }, cfg) }
}

// new_db constructs a lazily connected SQL handle using a third-party driver factory.
pub fn new_db(factory DriverFactory, cfg PoolConfig) &DB {
	return &DB{ pool: new_pool(factory, cfg) }
}

// acquire pins a connection for multiple statements or backend-specific SQL transactions.
pub fn (mut database DB) acquire() !&Conn {
	return database.pool.acquire()
}

// exec executes query on an acquired connection and returns it to the pool.
pub fn (mut database DB) exec(query string) ![]DriverRow {
	mut c := database.acquire()!
	defer { c.close() or {} }
	return c.exec(query)
}

// exec_one returns one row and returns the acquired connection to the pool.
pub fn (mut database DB) exec_one(query string) !DriverRow {
	mut c := database.acquire()!
	defer { c.close() or {} }
	return c.exec_one(query)
}

// exec_param_many executes a parameterized query and releases the acquired connection.
pub fn (mut database DB) exec_param_many(query string, params []string) ![]DriverRow {
	mut c := database.acquire()!
	defer { c.close() or {} }
	return c.exec_param_many(query, params)
}

// ping verifies that a connection can be acquired and validated.
pub fn (mut database DB) ping() ! {
	mut c := database.acquire()!
	defer { c.close() or {} }
	if !c.validate()! { return error('db: connection is invalid') }
}

// close stops acquisition and closes idle connections; borrowed connections close on release.
pub fn (mut database DB) close() ! {
	database.pool.close()
}

// stats returns a consistent snapshot of pool counters.
pub fn (mut database DB) stats() PoolStats {
	return database.pool.stats()
}

// set_max_open_conns changes the physical connection limit; zero means unlimited.
pub fn (mut database DB) set_max_open_conns(n int) {
	database.pool.set_max_open(n)
}

// set_max_idle_conns changes the retained idle connection limit.
pub fn (mut database DB) set_max_idle_conns(n int) {
	database.pool.set_max_idle(n)
}

// set_conn_max_lifetime changes physical connection lifetime; zero disables expiration.
pub fn (mut database DB) set_conn_max_lifetime(d time.Duration) {
	database.pool.set_max_lifetime(d)
}
