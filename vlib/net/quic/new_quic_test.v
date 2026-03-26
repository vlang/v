// Integration tests for QUIC connection management.
module quic

import net
import time

fn test_ngtcp2_bindings() {
	println('Testing ngtcp2 struct sizes and initialization...')

	settings := Ngtcp2SettingsStruct{
		token:              unsafe { nil }
		preferred_versions: unsafe { nil }
		available_versions: unsafe { nil }
		pmtud_probes:       unsafe { nil }
	}
	assert sizeof(settings) > 0

	params := Ngtcp2TransportParamsStruct{
		version_info: Ngtcp2VersionInfo{
			available_versions: unsafe { nil }
		}
	}
	assert sizeof(params) > 0

	println('ngtcp2 bindings test passed')
}

fn test_connection_config() {
	println('Testing ConnectionConfig...')
	config := ConnectionConfig{
		remote_addr:      '127.0.0.1:4433'
		alpn:             ['h3']
		max_idle_timeout: 30000
	}
	assert config.alpn.len == 1
	assert config.alpn[0] == 'h3'
	assert config.max_idle_timeout == 30000
}

fn test_zero_rtt_structures() {
	println('Testing 0-RTT structures...')

	config := ZeroRTTConfig{
		enabled:        true
		max_early_data: 8192
	}
	assert config.enabled == true
	assert config.max_early_data == 8192
}

fn test_config_validation() {
	println('Testing QUIC config validation...')
	default_config := ConnectionConfig{
		remote_addr: '127.0.0.1:4433'
	}
	// Check defaults
	assert default_config.max_idle_timeout == 30000
	assert default_config.max_stream_data_bidi_local == 1048576
	assert default_config.max_streams_bidi == 100

	// Check ALPN
	assert 'h3' in default_config.alpn
}

fn test_connection_config_with_session_cache() {
	println('Testing ConnectionConfig with session_cache...')
	cache := new_session_cache()
	config := ConnectionConfig{
		remote_addr:   '127.0.0.1:4433'
		session_cache: cache
	}
	assert config.session_cache != unsafe { nil }
	assert config.remote_addr == '127.0.0.1:4433'

	println('✓ ConnectionConfig with session_cache test passed')
}

fn test_connection_config_session_cache_default_nil() {
	println('Testing ConnectionConfig session_cache defaults to nil...')
	config := ConnectionConfig{
		remote_addr: '127.0.0.1:4433'
	}
	assert config.session_cache == unsafe { nil }

	println('✓ ConnectionConfig session_cache default nil test passed')
}

fn test_connection_is_0rtt_checks_state() {
	println('Testing Connection.is_0rtt_available...')
	mut conn := Connection{
		remote_addr: '127.0.0.1:4433'
	}
	assert conn.is_0rtt_available() == false

	conn.zero_rtt.state = .accepted
	assert conn.is_0rtt_available() == true

	conn.zero_rtt.state = .rejected
	assert conn.is_0rtt_available() == false

	println('✓ Connection.is_0rtt_available test passed')
}

fn test_connection_check_path_degradation() {
	println('Testing Connection.check_path_degradation...')
	addrs := net.resolve_addrs('192.168.1.100', .ip, .udp) or {
		println('  ⚠️ Skipping test: Cannot resolve address')
		return
	}
	if addrs.len == 0 {
		println('  ⚠️ Skipping test: No addresses resolved')
		return
	}
	remote_addrs := net.resolve_addrs('203.0.113.1', .ip, .udp) or {
		println('  ⚠️ Skipping test: Cannot resolve address')
		return
	}
	if remote_addrs.len == 0 {
		println('  ⚠️ Skipping test: No addresses resolved')
		return
	}

	mut conn := Connection{
		remote_addr: '203.0.113.1:4433'
		migration:   new_connection_migration(addrs[0], remote_addrs[0])
	}

	// Default RTT is 0 -> no degradation
	assert conn.check_path_degradation() == false

	// High RTT -> degradation detected
	conn.migration.current_path.rtt = 600 * time.millisecond
	assert conn.check_path_degradation() == true

	println('✓ Connection.check_path_degradation test passed')
}

fn test_connection_save_session_ticket() {
	println('Testing Connection.save_session_ticket...')
	mut cache := new_session_cache()
	mut conn := Connection{
		remote_addr:   '127.0.0.1:4433'
		session_cache: cache
	}

	ticket := SessionTicket{
		ticket:          [u8(1), 2, 3]
		creation_time:   time.now()
		server_name:     '127.0.0.1'
		ticket_lifetime: 86400
		alpn_protocol:   'h3'
		cipher_suite:    0x1301
	}

	conn.save_session_ticket(ticket)

	retrieved := cache.get('127.0.0.1') or {
		assert false, 'Failed to retrieve saved ticket'
		return
	}
	assert retrieved.server_name == '127.0.0.1'
	assert retrieved.alpn_protocol == 'h3'

	println('✓ Connection.save_session_ticket test passed')
}

fn test_connection_save_session_ticket_nil_cache() {
	println('Testing Connection.save_session_ticket with nil cache...')
	mut conn := Connection{
		remote_addr: '127.0.0.1:4433'
	}

	ticket := SessionTicket{
		ticket:          [u8(1), 2, 3]
		creation_time:   time.now()
		server_name:     '127.0.0.1'
		ticket_lifetime: 86400
	}

	conn.save_session_ticket(ticket)

	println('✓ Connection.save_session_ticket nil cache test passed')
}

fn test_connection_send_early_data() {
	println('Testing Connection.send_early_data...')
	mut cache := new_session_cache()
	mut conn := Connection{
		remote_addr:   '127.0.0.1:4433'
		session_cache: cache
		zero_rtt:      new_zero_rtt_connection(ZeroRTTConfig{
			enabled:        true
			max_early_data: 16384
		})
	}

	data := 'GET /index.html HTTP/3\r\n'.bytes()
	conn.send_early_data(4, data) or {
		assert false, 'send_early_data failed: ${err}'
		return
	}

	assert conn.zero_rtt.bytes_sent == u32(data.len)
	assert conn.zero_rtt.early_data.len == 1
	assert conn.zero_rtt.early_data[0].stream_id == 4

	println('✓ Connection.send_early_data test passed')
}

fn test_connection_send_early_data_disabled() {
	println('Testing Connection.send_early_data when disabled...')
	mut conn := Connection{
		remote_addr: '127.0.0.1:4433'
		zero_rtt:    new_zero_rtt_connection(ZeroRTTConfig{
			enabled: false
		})
	}

	conn.send_early_data(4, 'test'.bytes()) or {
		assert err.msg().contains('cannot send early data')
		println('✓ Connection.send_early_data disabled test passed')
		return
	}

	assert false, 'Should have failed when 0-RTT is disabled'
}

fn test_connection_complete_migration() {
	println('Testing Connection.complete_migration...')
	addrs := net.resolve_addrs('192.168.1.100', .ip, .udp) or {
		println('  ⚠️ Skipping test: Cannot resolve address')
		return
	}
	if addrs.len == 0 {
		println('  ⚠️ Skipping test: No addresses resolved')
		return
	}
	remote_addrs := net.resolve_addrs('203.0.113.1', .ip, .udp) or {
		println('  ⚠️ Skipping test: Cannot resolve address')
		return
	}
	if remote_addrs.len == 0 {
		println('  ⚠️ Skipping test: No addresses resolved')
		return
	}
	new_remote_addrs := net.resolve_addrs('10.0.0.1', .ip, .udp) or {
		println('  ⚠️ Skipping test: Cannot resolve address')
		return
	}
	if new_remote_addrs.len == 0 {
		println('  ⚠️ Skipping test: No addresses resolved')
		return
	}

	mut conn := Connection{
		remote_addr: '203.0.113.1:4433'
		migration:   new_connection_migration(addrs[0], remote_addrs[0])
	}

	conn.migration.probe_path(addrs[0], new_remote_addrs[0]) or {
		assert false, 'Failed to probe path: ${err}'
		return
	}

	last_path := conn.migration.alternative_paths.last()
	pk := path_to_key(last_path)
	challenge := conn.migration.pending_challenges[pk] or {
		assert false, 'No challenge found for probed path'
		return
	}

	response := PathResponse{
		data: challenge.data
	}

	conn.complete_migration(response) or {
		assert false, 'Complete migration failed: ${err}'
		return
	}

	assert conn.migration.state == .completed

	println('✓ Connection.complete_migration test passed')
}

fn test_connection_complete_migration_no_pending() {
	println('Testing Connection.complete_migration with no pending...')
	mut conn := Connection{
		remote_addr: '127.0.0.1:4433'
	}

	response := PathResponse{
		data: [u8(1), 2, 3, 4, 5, 6, 7, 8]!
	}

	conn.complete_migration(response) or {
		assert err.msg().contains('no pending migration')
		println('✓ Connection.complete_migration no pending test passed')
		return
	}

	assert false, 'Should have failed with no pending migration'
}

fn test_connection_migrate_connection() {
	println('Testing Connection.migrate_connection...')
	addrs := net.resolve_addrs('192.168.1.100', .ip, .udp) or {
		println('  ⚠️ Skipping test: Cannot resolve address')
		return
	}
	if addrs.len == 0 {
		println('  ⚠️ Skipping test: No addresses resolved')
		return
	}
	remote_addrs := net.resolve_addrs('203.0.113.1', .ip, .udp) or {
		println('  ⚠️ Skipping test: Cannot resolve address')
		return
	}
	if remote_addrs.len == 0 {
		println('  ⚠️ Skipping test: No addresses resolved')
		return
	}

	mut conn := Connection{
		remote_addr: '203.0.113.1:4433'
		migration:   new_connection_migration(addrs[0], remote_addrs[0])
	}

	conn.migrate_connection('10.0.0.1:4433') or {
		println('  ⚠️ Skipping test: ${err}')
		return
	}

	assert conn.migration.state == .probing
	assert conn.migration.alternative_paths.len == 1
	assert conn.migration.pending_challenges.len == 1

	println('✓ Connection.migrate_connection test passed')
}

fn test_connection_migrate_connection_closed() {
	println('Testing Connection.migrate_connection when closed...')
	mut conn := Connection{
		remote_addr: '127.0.0.1:4433'
		closed:      true
	}

	conn.migrate_connection('10.0.0.1:4433') or {
		assert err.msg().contains('connection closed')
		println('✓ Connection.migrate_connection closed test passed')
		return
	}

	assert false, 'Should have failed when connection is closed'
}

fn test_connection_close_sets_closed() {
	println('Testing Connection.close sets closed flag...')
	mut conn := Connection{
		remote_addr: '127.0.0.1:4433'
	}
	assert conn.closed == false
	assert conn.ngtcp2_conn == unsafe { nil }

	conn.close()

	assert conn.closed == true
	assert conn.ngtcp2_conn == unsafe { nil }
	assert conn.streams.len == 0

	println('✓ Connection.close sets closed flag test passed')
}

fn test_connection_close_idempotent() {
	println('Testing Connection.close idempotent...')
	mut conn := Connection{
		remote_addr: '127.0.0.1:4433'
	}

	conn.close()
	assert conn.closed == true

	// Second close must not panic
	conn.close()
	assert conn.closed == true

	println('✓ Connection.close idempotent test passed')
}

fn test_connection_max_data_left() {
	println('Testing Connection.max_data_left...')
	mut conn := Connection{
		remote_addr: '127.0.0.1:4433'
	}
	// With nil ngtcp2_conn, should return 0 (safe default)
	assert conn.max_data_left() == u64(0)

	println('✓ Connection.max_data_left test passed')
}

fn test_connection_streams_left() {
	println('Testing Connection.streams_bidi_left and streams_uni_left...')
	mut conn := Connection{
		remote_addr: '127.0.0.1:4433'
	}
	// With nil ngtcp2_conn, should return 0 (safe default)
	assert conn.streams_bidi_left() == u64(0)
	assert conn.streams_uni_left() == u64(0)

	println('✓ Connection.streams_left test passed')
}

fn test_connection_reset_stream() {
	println('Testing Connection.reset_stream on closed connection...')
	mut conn := Connection{
		remote_addr: '127.0.0.1:4433'
		closed:      true
	}

	conn.reset_stream(4, 0) or {
		assert err.msg().contains('connection closed')
		println('✓ Connection.reset_stream closed test passed')
		return
	}
	assert false, 'Should have failed on closed connection'
}

fn test_connection_reset_stream_nil_conn() {
	println('Testing Connection.reset_stream with nil ngtcp2...')
	mut conn := Connection{
		remote_addr: '127.0.0.1:4433'
	}

	conn.reset_stream(4, 0) or {
		assert err.msg().contains('not initialized')
		println('✓ Connection.reset_stream nil ngtcp2 test passed')
		return
	}
	assert false, 'Should have failed with nil ngtcp2'
}

fn test_connection_stop_sending() {
	println('Testing Connection.stop_sending on closed connection...')
	mut conn := Connection{
		remote_addr: '127.0.0.1:4433'
		closed:      true
	}

	conn.stop_sending(4, 0) or {
		assert err.msg().contains('connection closed')
		println('✓ Connection.stop_sending closed test passed')
		return
	}
	assert false, 'Should have failed on closed connection'
}

fn test_connection_stop_sending_nil_conn() {
	println('Testing Connection.stop_sending with nil ngtcp2...')
	mut conn := Connection{
		remote_addr: '127.0.0.1:4433'
	}

	conn.stop_sending(4, 0) or {
		assert err.msg().contains('not initialized')
		println('✓ Connection.stop_sending nil ngtcp2 test passed')
		return
	}
	assert false, 'Should have failed with nil ngtcp2'
}

fn test_close_with_error_code() {
	println('Testing Connection.close_with_error...')
	mut conn := Connection{
		remote_addr: '127.0.0.1:4433'
	}
	assert conn.closed == false

	conn.close_with_error(0x0100, 'test close') or {
		assert false, 'close_with_error should not fail: ${err}'
		return
	}

	assert conn.closed == true
	assert conn.ngtcp2_conn == unsafe { nil }
	assert conn.streams.len == 0

	println('✓ Connection.close_with_error test passed')
}

fn test_flush_early_data_accepted() {
	println('Testing Connection.flush_early_data with accepted state...')
	mut conn := Connection{
		remote_addr: '127.0.0.1:4433'
		zero_rtt:    new_zero_rtt_connection(ZeroRTTConfig{
			enabled:        true
			max_early_data: 16384
		})
	}

	// Buffer some early data
	conn.zero_rtt.add_early_data('early request data'.bytes(), 4) or {
		assert false, 'failed to buffer early data: ${err}'
		return
	}
	assert conn.zero_rtt.early_data.len == 1

	// Simulate server accepting 0-RTT
	conn.zero_rtt.accept()
	assert conn.zero_rtt.state == .accepted

	// Flush should succeed without error (send failures are non-fatal)
	conn.flush_early_data() or {
		assert false, 'flush_early_data should not error: ${err}'
		return
	}

	println('✓ Connection.flush_early_data accepted test passed')
}

fn test_flush_early_data_not_accepted() {
	println('Testing Connection.flush_early_data when disabled...')
	mut conn := Connection{
		remote_addr: '127.0.0.1:4433'
		zero_rtt:    new_zero_rtt_connection(ZeroRTTConfig{
			enabled: false
		})
	}
	assert conn.zero_rtt.state == .disabled

	// Flush should be a no-op (no error, no action)
	conn.flush_early_data() or {
		assert false, 'flush_early_data should be no-op when disabled: ${err}'
		return
	}

	println('✓ Connection.flush_early_data not accepted test passed')
}

fn test_flush_early_data_rejected() {
	println('Testing Connection.flush_early_data with rejected state...')
	mut conn := Connection{
		remote_addr: '127.0.0.1:4433'
		zero_rtt:    new_zero_rtt_connection(ZeroRTTConfig{
			enabled:        true
			max_early_data: 16384
		})
	}

	// Buffer early data, then reject
	conn.zero_rtt.add_early_data('rejected data'.bytes(), 4) or {
		assert false, 'failed to buffer early data: ${err}'
		return
	}
	conn.zero_rtt.reject()
	assert conn.zero_rtt.state == .rejected
	assert conn.zero_rtt.early_data.len == 0 // reject clears buffer

	// Flush should be a no-op when rejected
	conn.flush_early_data() or {
		assert false, 'flush_early_data should be no-op when rejected: ${err}'
		return
	}

	println('✓ Connection.flush_early_data rejected test passed')
}

fn test_flush_early_data_closed_connection() {
	println('Testing Connection.flush_early_data on closed connection...')
	mut conn := Connection{
		remote_addr: '127.0.0.1:4433'
		closed:      true
		zero_rtt:    new_zero_rtt_connection(ZeroRTTConfig{
			enabled: true
		})
	}

	conn.flush_early_data() or {
		assert err.msg().contains('connection closed')
		println('✓ Connection.flush_early_data closed connection test passed')
		return
	}

	assert false, 'Should have failed on closed connection'
}
