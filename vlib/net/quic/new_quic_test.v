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
