// Connection Migration Tests
module quic

import net
import time

fn test_connection_id_equals() {
	id1 := new_connection_id([u8(1), 2, 3, 4])
	id2 := new_connection_id([u8(1), 2, 3, 4])
	id3 := new_connection_id([u8(5), 6, 7, 8])

	assert id1.equals(id2) == true
	assert id1.equals(id3) == false

	println('✓ Connection ID equals test passed')
}

fn test_path_info_creation() {
	addrs := net.resolve_addrs('192.168.1.100', .ip, .udp) or {
		println('  ⚠️ Skipping test: Cannot resolve address')
		return
	}
	if addrs.len == 0 {
		println('  ⚠️ Skipping test: No addresses resolved')
		return
	}
	local_addr := addrs[0]

	remote_addrs := net.resolve_addrs('203.0.113.1', .ip, .udp) or {
		println('  ⚠️ Skipping test: Cannot resolve address')
		return
	}
	if remote_addrs.len == 0 {
		println('  ⚠️ Skipping test: No addresses resolved')
		return
	}
	remote_addr := remote_addrs[0]

	path := new_path_info(local_addr, remote_addr)

	assert path.validated == false
	assert path.active == false
	assert path.mtu == 1200

	println('✓ Path info creation test passed')
}

fn test_connection_migration_probe_path() {
	local_addr := net.resolve_ipaddr('192.168.1.100', .ip, .udp) or {
		println('  ⚠️ Skipping test: Cannot resolve address')
		return
	}
	remote_addr := net.resolve_ipaddr('203.0.113.1', .ip, .udp) or {
		println('  ⚠️ Skipping test: Cannot resolve address')
		return
	}

	mut migration := new_connection_migration(local_addr, remote_addr)

	// Probe new path
	new_local := net.resolve_ipaddr('10.0.0.50', .ip, .udp) or {
		println('  ⚠️ Skipping test: Cannot resolve address')
		return
	}
	new_path := migration.probe_path(new_local, remote_addr) or {
		assert false, 'Failed to probe path: ${err}'
		return
	}

	assert migration.state == .probing
	assert migration.alternative_paths.len == 1
	assert migration.pending_challenges.len == 1

	println('✓ Connection migration probe path test passed')
}

fn test_connection_migration_max_paths() {
	local_addr := net.resolve_ipaddr('192.168.1.100', .ip, .udp) or {
		println('  ⚠️ Skipping test: Cannot resolve address')
		return
	}
	remote_addr := net.resolve_ipaddr('203.0.113.1', .ip, .udp) or {
		println('  ⚠️ Skipping test: Cannot resolve address')
		return
	}

	mut migration := new_connection_migration(local_addr, remote_addr)
	migration.max_paths = 2

	// Add paths up to limit
	new_local1 := net.resolve_ipaddr('10.0.0.50', .ip, .udp) or {
		println('  ⚠️ Skipping test: Cannot resolve address')
		return
	}
	migration.probe_path(new_local1, remote_addr) or {
		assert false, 'Failed to probe path 1'
		return
	}

	new_local2 := net.resolve_ipaddr('10.0.0.51', .ip, .udp) or {
		println('  ⚠️ Skipping test: Cannot resolve address')
		return
	}
	migration.probe_path(new_local2, remote_addr) or {
		assert false, 'Failed to probe path 2'
		return
	}

	// Should fail when exceeding max
	new_local3 := net.resolve_ipaddr('10.0.0.52', .ip, .udp) or {
		println('  ⚠️ Skipping test: Cannot resolve address')
		return
	}
	result := migration.probe_path(new_local3, remote_addr)

	if result is PathInfo {
		assert false, 'Should have failed with max paths error'
	}

	println('✓ Connection migration max paths test passed')
}

fn test_path_degradation_detection() {
	local_addr := net.resolve_ipaddr('192.168.1.100', .ip, .udp) or {
		println('  ⚠️ Skipping test: Cannot resolve address')
		return
	}
	remote_addr := net.resolve_ipaddr('203.0.113.1', .ip, .udp) or {
		println('  ⚠️ Skipping test: Cannot resolve address')
		return
	}

	migration := new_connection_migration(local_addr, remote_addr)

	// Test high packet loss
	degraded1 := migration.detect_path_degradation(0.06, 100 * time.millisecond)
	assert degraded1 == true

	// Test high RTT
	degraded2 := migration.detect_path_degradation(0.01, 600 * time.millisecond)
	assert degraded2 == true

	// Test good conditions
	degraded3 := migration.detect_path_degradation(0.01, 50 * time.millisecond)
	assert degraded3 == false

	println('✓ Path degradation detection test passed')
}

fn test_migration_stats() {
	local_addr := net.resolve_ipaddr('192.168.1.100', .ip, .udp) or {
		println('  ⚠️ Skipping test: Cannot resolve address')
		return
	}
	remote_addr := net.resolve_ipaddr('203.0.113.1', .ip, .udp) or {
		println('  ⚠️ Skipping test: Cannot resolve address')
		return
	}

	mut migration := new_connection_migration(local_addr, remote_addr)

	// Add some migration events
	event1 := MigrationEvent{
		reason:    .network_change
		old_path:  migration.current_path
		new_path:  migration.current_path
		timestamp: time.now()
		success:   true
	}
	migration.migration_history << event1

	event2 := MigrationEvent{
		reason:    .nat_rebinding
		old_path:  migration.current_path
		new_path:  migration.current_path
		timestamp: time.now()
		success:   true
	}
	migration.migration_history << event2

	stats := migration.get_migration_stats()

	assert stats.total_migrations == 2
	assert stats.successful_migrations == 2
	assert stats.network_changes == 1
	assert stats.nat_rebindings == 1
	assert stats.success_rate() == 1.0

	println('✓ Migration stats test passed')
}

fn test_migration_controller() {
	local_addr := net.resolve_ipaddr('192.168.1.100', .ip, .udp) or {
		println('  ⚠️ Skipping test: Cannot resolve address')
		return
	}
	remote_addr := net.resolve_ipaddr('203.0.113.1', .ip, .udp) or {
		println('  ⚠️ Skipping test: Cannot resolve address')
		return
	}

	policy := MigrationPolicy{
		auto_migrate_on_network_change: true
		auto_migrate_on_degradation:    true
		packet_loss_threshold:          0.05
		rtt_threshold:                  500 * time.millisecond
	}

	mut controller := new_migration_controller(local_addr, remote_addr, policy)

	assert controller.policy.auto_migrate_on_degradation == true
	assert controller.migration.enabled == true

	println('✓ Migration controller test passed')
}

fn test_handle_nat_rebinding() {
	local_addr := net.resolve_ipaddr('192.168.1.100', .ip, .udp) or {
		println('  ⚠️ Skipping test: Cannot resolve address')
		return
	}
	remote_addr := net.resolve_ipaddr('203.0.113.1', .ip, .udp) or {
		println('  ⚠️ Skipping test: Cannot resolve address')
		return
	}

	mut migration := new_connection_migration(local_addr, remote_addr)

	new_remote_addr := net.resolve_ipaddr('203.0.113.1', .ip, .udp) or {
		println('  ⚠️ Skipping test: Cannot resolve address')
		return
	}
	migration.handle_nat_rebinding(new_remote_addr) or {
		assert false, 'NAT rebinding failed: ${err}'
		return
	}

	assert migration.migration_history.len == 1
	assert migration.migration_history[0].reason == .nat_rebinding

	println('✓ NAT rebinding test passed')
}
