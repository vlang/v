module v3

// HTTP/3 connection pool for reusing connections per RFC 9114 §3.3.
import sync

// ClientPool manages a pool of HTTP/3 client connections keyed by "host:port".
// Connections can be reused for different origins on the same server per RFC 9114 §3.3.
@[heap]
pub struct ClientPool {
mut:
	connections map[string]&Client
	mu          &sync.Mutex = sync.new_mutex()
	max_idle    int         = 10
}

// new_client_pool creates a new HTTP/3 client pool with the given max idle connections.
pub fn new_client_pool(max_idle int) &ClientPool {
	return &ClientPool{
		max_idle: max_idle
	}
}

// get_or_create returns an existing pooled connection for the address,
// or creates a new one via QUIC handshake.
pub fn (mut p ClientPool) get_or_create(address string) !&Client {
	p.mu.lock()
	if client := p.connections[address] {
		p.mu.unlock()
		return client
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
pub fn (mut p ClientPool) release(address string) {
	// No-op: connection remains in pool.
}

// close_all closes all pooled connections and empties the pool.
pub fn (mut p ClientPool) close_all() {
	p.mu.lock()
	for _, mut client in p.connections {
		client.close()
	}
	p.connections.clear()
	p.mu.unlock()
}

// remove removes and closes a specific connection from the pool.
pub fn (mut p ClientPool) remove(address string) {
	p.mu.lock()
	if mut client := p.connections[address] {
		client.close()
	}
	p.connections.delete(address)
	p.mu.unlock()
}

// size returns the number of pooled connections.
pub fn (p &ClientPool) size() int {
	return p.connections.len
}
