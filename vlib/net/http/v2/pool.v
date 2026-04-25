module v2

// HTTP/2 connection pool for reusing connections per RFC 7540 §9.1.1.
import sync

// ConnectionPool manages a pool of HTTP/2 client connections keyed by "host:port".
// Connections SHOULD be reused for the same origin per RFC 7540 §9.1.1.
@[heap]
pub struct ConnectionPool {
mut:
	connections map[string]&Client
	mu          &sync.Mutex = sync.new_mutex()
	max_idle    int         = 10
}

// new_connection_pool creates a new connection pool with the given max idle connections.
pub fn new_connection_pool(max_idle int) &ConnectionPool {
	return &ConnectionPool{
		max_idle: max_idle
	}
}

// get_or_create returns an existing pooled connection for the address,
// or creates a new one via TLS+ALPN handshake.
pub fn (mut p ConnectionPool) get_or_create(address string) !&Client {
	p.mu.lock()
	if client := p.connections[address] {
		if !client.conn.closed {
			p.mu.unlock()
			return client
		}
		// Connection is stale, remove it and fall through to create new
		p.connections.delete(address)
	}

	c := new_client(address) or {
		p.mu.unlock()
		return err
	}

	p.connections[address] = &c
	client := p.connections[address] or {
		p.mu.unlock()
		return error('pool: failed to store connection')
	}
	p.mu.unlock()
	return client
}

// release marks a connection as available in the pool.
// Currently a no-op: the connection stays in the pool until explicitly
// removed or the pool is closed.
pub fn (mut p ConnectionPool) release(address string) {
	// No-op: connection remains in pool.
}

// close_all closes all pooled connections and empties the pool.
pub fn (mut p ConnectionPool) close_all() {
	p.mu.lock()
	for _, mut client in p.connections {
		client.close()
	}
	p.connections.clear()
	p.mu.unlock()
}

// remove removes and closes a specific connection from the pool.
pub fn (mut p ConnectionPool) remove(address string) {
	p.mu.lock()
	if mut client := p.connections[address] {
		client.close()
	}
	p.connections.delete(address)
	p.mu.unlock()
}

// size returns the number of pooled connections.
pub fn (mut p ConnectionPool) size() int {
	p.mu.lock()
	defer {
		p.mu.unlock()
	}
	return p.connections.len
}
