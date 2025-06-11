module main

import db.pg

pub struct ConnectionPool {
mut:
	connections chan pg.DB
	config      pg.Config
}

// new_connection_pool creates a new connection pool with the given size and configuration.
pub fn new_connection_pool(config pg.Config, size int) !ConnectionPool {
	mut connections := chan pg.DB{cap: size}
	for _ in 0 .. size {
		conn := pg.connect(config)!
		connections <- conn
	}
	return ConnectionPool{
		connections: connections
		config:      config
	}
}

// acquire gets a connection from the pool
pub fn (mut pool ConnectionPool) acquire() !pg.DB {
	return <-pool.connections or { return error('Failed to acquire a connection from the pool') }
}

// release returns a connection back to the pool.
pub fn (mut pool ConnectionPool) release(conn pg.DB) {
	pool.connections <- conn
}

// close closes all connections in the pool.
pub fn (mut pool ConnectionPool) close() {
	for _ in 0 .. pool.connections.len {
		mut conn := <-pool.connections or { break }
		conn.close()
	}
}
