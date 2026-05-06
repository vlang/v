module v3

// Tests for HTTP/3 connection pool (RFC 9114 §3.3).

fn test_pool_size() {
	mut pool := new_client_pool(10)
	assert pool.size() == 0, 'new pool should have size 0, got ${pool.size()}'
}

fn test_pool_get_or_create_reuses() {
	mut pool := new_client_pool(10)
	mock := &Client{}
	pool.connections['localhost:443'] = mock

	client := pool.get_or_create('localhost:443') or {
		assert false, 'expected to get existing client, got error: ${err}'
		return
	}
	assert voidptr(client) == voidptr(mock), 'should return the same pooled client'
	assert pool.size() == 1
}

fn test_pool_get_or_create_new() {
	mut pool := new_client_pool(10)
	// No server at this address — connection should fail
	pool.get_or_create('127.0.0.1:1') or {
		assert pool.size() == 0, 'pool should remain empty after failed connection'
		return
	}
	// If connection somehow succeeds, verify pool grew
	assert pool.size() == 1
}

fn test_pool_remove() {
	mut pool := new_client_pool(10)
	pool.connections['host:443'] = &Client{}
	assert pool.size() == 1, 'pool should have 1 connection after insert'

	pool.remove('host:443')
	assert pool.size() == 0, 'pool should be empty after remove'
}

fn test_pool_close_all() {
	mut pool := new_client_pool(10)
	pool.connections['host1:443'] = &Client{}
	pool.connections['host2:443'] = &Client{}
	assert pool.size() == 2, 'pool should have 2 connections'

	pool.close_all()
	assert pool.size() == 0, 'pool should be empty after close_all'
}
