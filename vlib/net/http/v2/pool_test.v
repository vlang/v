module v2

// Tests for HTTP/2 connection pool (RFC 7540 §9.1.1).

fn test_pool_size() {
	mut pool := new_connection_pool(10)
	assert pool.size() == 0, 'new pool should have size 0, got ${pool.size()}'
}

fn test_pool_get_or_create_reuses() {
	mut pool := new_connection_pool(10)
	// Insert a mock client directly (conn.closed=true prevents close() from using nil ssl_conn)
	mock := &Client{
		conn: Connection{
			closed: true
		}
	}
	pool.connections['localhost:443'] = mock

	client := pool.get_or_create('localhost:443') or {
		assert false, 'expected to get existing client, got error: ${err}'
		return
	}
	assert voidptr(client) == voidptr(mock), 'should return the same pooled client'
	assert pool.size() == 1
}

fn test_pool_get_or_create_new() {
	mut pool := new_connection_pool(10)
	// No server at this address — connection should fail
	pool.get_or_create('127.0.0.1:1') or {
		assert pool.size() == 0, 'pool should remain empty after failed connection'
		return
	}
	// If connection somehow succeeds, verify pool grew
	assert pool.size() == 1
}

fn test_pool_remove() {
	mut pool := new_connection_pool(10)
	pool.connections['host:443'] = &Client{
		conn: Connection{
			closed: true
		}
	}
	assert pool.size() == 1, 'pool should have 1 connection after insert'

	pool.remove('host:443')
	assert pool.size() == 0, 'pool should be empty after remove'
}

fn test_pool_close_all() {
	mut pool := new_connection_pool(10)
	pool.connections['host1:443'] = &Client{
		conn: Connection{
			closed: true
		}
	}
	pool.connections['host2:443'] = &Client{
		conn: Connection{
			closed: true
		}
	}
	assert pool.size() == 2, 'pool should have 2 connections'

	pool.close_all()
	assert pool.size() == 0, 'pool should be empty after close_all'
}
