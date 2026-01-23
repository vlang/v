// 0-RTT Connection Resumption Tests
module quic

import time

fn test_session_cache_store_and_get() {
	mut cache := new_session_cache()

	ticket := SessionTicket{
		ticket:          [u8(1), 2, 3, 4, 5]
		creation_time:   time.now()
		max_early_data:  16384
		alpn_protocol:   'h3'
		server_name:     'example.com'
		cipher_suite:    0x1301
		ticket_lifetime: 86400
	}

	cache.store('example.com', ticket)

	retrieved := cache.get('example.com') or {
		assert false, 'Failed to retrieve ticket'
		return
	}

	assert retrieved.server_name == 'example.com'
	assert retrieved.alpn_protocol == 'h3'
	assert retrieved.max_early_data == 16384

	println('✓ Session cache store and get test passed')
}

fn test_session_cache_expiration() {
	mut cache := new_session_cache()

	// Create expired ticket (25 hours ago)
	old_time := time.now().add(-25 * time.hour)
	mut ticket := SessionTicket{
		ticket:          [u8(1), 2, 3, 4, 5]
		creation_time:   old_time
		max_early_data:  16384
		alpn_protocol:   'h3'
		server_name:     'example.com'
		cipher_suite:    0x1301
		ticket_lifetime: 86400 // 24 hours
	}

	cache.store('example.com', ticket)

	// Should not retrieve expired ticket
	retrieved := cache.get('example.com')
	assert retrieved == none

	println('✓ Session cache expiration test passed')
}

fn test_zero_rtt_connection_early_data() {
	config := ZeroRTTConfig{
		enabled:        true
		max_early_data: 16384
	}

	mut conn := new_zero_rtt_connection(config)

	assert conn.can_send_early_data() == true

	data := 'GET /api/data HTTP/3\r\n'.bytes()
	result := conn.add_early_data(data, 1) or {
		assert false, 'Failed to add early data: ${err}'
		return
	}

	assert result == true
	assert conn.bytes_sent == u32(data.len)

	early_data := conn.get_early_data()
	assert early_data.len == 1
	assert early_data[0].stream_id == 1

	println('✓ 0-RTT early data test passed')
}

fn test_zero_rtt_max_early_data_limit() {
	config := ZeroRTTConfig{
		enabled:        true
		max_early_data: 100
	}

	mut conn := new_zero_rtt_connection(config)

	// Try to send more than max_early_data
	large_data := []u8{len: 200}
	result := conn.add_early_data(large_data, 1) or {
		// Should fail
		assert err.msg().contains('exceeds maximum')
		println('✓ 0-RTT max early data limit test passed')
		return
	}

	assert false, 'Should have failed with size limit error'
}

fn test_zero_rtt_accept_reject() {
	config := ZeroRTTConfig{
		enabled:        true
		max_early_data: 16384
	}

	mut conn := new_zero_rtt_connection(config)

	data := 'test data'.bytes()
	conn.add_early_data(data, 1) or {
		assert false, 'Failed to add early data'
		return
	}

	// Test accept
	conn.accept()
	assert conn.state == .accepted

	// Test reject
	mut conn2 := new_zero_rtt_connection(config)
	conn2.add_early_data(data, 1) or {
		assert false, 'Failed to add early data'
		return
	}
	conn2.reject()
	assert conn2.state == .rejected
	assert conn2.bytes_sent == 0
	assert conn2.early_data.len == 0

	println('✓ 0-RTT accept/reject test passed')
}

fn test_anti_replay_cache() {
	mut cache := new_anti_replay_cache()

	token := 'unique-token-123'

	// First check should succeed
	result1 := cache.check_and_store(token)
	assert result1 == true

	// Second check with same token should fail (replay detected)
	result2 := cache.check_and_store(token)
	assert result2 == false

	println('✓ Anti-replay cache test passed')
}

fn test_zero_rtt_stats() {
	mut stats := ZeroRTTStats{}

	stats.record_attempt()
	stats.record_accepted(1000)
	stats.record_attempt()
	stats.record_rejected()
	stats.record_attempt()
	stats.record_accepted(2000)

	assert stats.attempts == 3
	assert stats.accepted == 2
	assert stats.rejected == 1
	assert stats.bytes_sent == 3000

	rate := stats.acceptance_rate()
	assert rate > 0.66 && rate < 0.67

	println('✓ 0-RTT stats test passed')
}
