// HTTP/3 QPACK, 0-RTT, and Connection Migration Example
// Demonstrates the new HTTP/3 features

module main

import net
import net.http.v3
import net.quic
import time

fn main() {
	println('=== HTTP/3 Advanced Features Demo ===\n')

	// Demo 1: QPACK Header Compression
	demo_qpack_compression()

	// Demo 2: 0-RTT Connection Resumption
	demo_zero_rtt()

	// Demo 3: Connection Migration
	demo_connection_migration()

	println('\n=== Demo Complete ===')
}

fn demo_qpack_compression() {
	println('Demo 1: QPACK Header Compression')
	println('==================================================')

	// Create QPACK encoder and decoder
	mut encoder := v3.new_qpack_encoder(4096, 100)
	mut decoder := v3.new_qpack_decoder(4096, 100)

	// Create sample headers
	headers := [
		v3.HeaderField{
			name:  ':method'
			value: 'GET'
		},
		v3.HeaderField{
			name:  ':scheme'
			value: 'https'
		},
		v3.HeaderField{
			name:  ':path'
			value: '/api/v1/users'
		},
		v3.HeaderField{
			name:  ':authority'
			value: 'api.example.com'
		},
		v3.HeaderField{
			name:  'user-agent'
			value: 'V-HTTP3-Client/1.0'
		},
		v3.HeaderField{
			name:  'accept'
			value: 'application/json'
		},
		v3.HeaderField{
			name:  'accept-encoding'
			value: 'gzip, deflate, br'
		},
		v3.HeaderField{
			name:  'authorization'
			value: 'Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9'
		},
	]

	// Calculate original size
	mut original_size := 0
	for header in headers {
		original_size += header.name.len + header.value.len + 2
	}

	// Encode headers
	start := time.now()
	encoded := encoder.encode(headers)
	encode_time := time.now() - start

	// Decode headers
	start2 := time.now()
	_ := decoder.decode(encoded) or {
		println('  ❌ Decoding failed: ${err}')
		return
	}
	decode_time := time.now() - start2

	// Calculate compression ratio
	compression_ratio := f64(original_size) / f64(encoded.len)

	println('  Original headers size: ${original_size} bytes')
	println('  Compressed size: ${encoded.len} bytes')
	println('  Compression ratio: ${compression_ratio:.2f}x')
	println('  Bandwidth savings: ${(1.0 - f64(encoded.len) / f64(original_size)) * 100:.1f}%')
	println('  Encoding time: ${encode_time.microseconds()} μs')
	println('  Decoding time: ${decode_time.microseconds()} μs')
	println('  ✓ Successfully encoded and decoded ${headers.len} headers')
	println('')
}

fn demo_zero_rtt() {
	println('Demo 2: 0-RTT Connection Resumption')
	println('==================================================')

	// Create session cache
	mut cache := quic.new_session_cache()

	// Simulate receiving a session ticket from server
	ticket := quic.SessionTicket{
		ticket:          [u8(1), 2, 3, 4, 5, 6, 7, 8]
		creation_time:   time.now()
		max_early_data:  16384
		alpn_protocol:   'h3'
		server_name:     'api.example.com'
		cipher_suite:    0x1301 // TLS_AES_128_GCM_SHA256
		ticket_lifetime: 86400  // 24 hours
	}

	// Store ticket in cache
	cache.store('api.example.com', ticket)
	println('  ✓ Session ticket stored for api.example.com')

	// Retrieve ticket for 0-RTT connection
	retrieved_ticket := cache.get('api.example.com') or {
		println('  ❌ Failed to retrieve ticket')
		return
	}
	println('  ✓ Session ticket retrieved successfully')
	println('  Ticket lifetime: ${retrieved_ticket.ticket_lifetime} seconds')
	println('  Max early data: ${retrieved_ticket.max_early_data} bytes')
	println('  ALPN protocol: ${retrieved_ticket.alpn_protocol}')

	// Create 0-RTT connection
	config := quic.ZeroRTTConfig{
		enabled:        true
		max_early_data: 16384
		anti_replay:    true
	}

	mut conn := quic.new_zero_rtt_connection(config)
	println('  ✓ 0-RTT connection created')

	// Add early data
	early_data := 'GET /api/data HTTP/3\r\nHost: api.example.com\r\n\r\n'.bytes()
	conn.add_early_data(early_data, 1) or {
		println('  ❌ Failed to add early data: ${err}')
		return
	}
	println('  ✓ Early data added (${early_data.len} bytes)')
	println('  Can send more early data: ${conn.can_send_early_data()}')
	println('  Bytes sent: ${conn.bytes_sent} / ${conn.max_early_data}')

	// Simulate server accepting 0-RTT
	conn.accept()
	println('  ✓ 0-RTT accepted by server')
	println('  Connection state: ${conn.state}')

	// Show statistics
	mut stats := quic.ZeroRTTStats{}
	stats.record_attempt()
	stats.record_accepted(u32(early_data.len))
	println('  Statistics:')
	println('    Attempts: ${stats.attempts}')
	println('    Accepted: ${stats.accepted}')
	println('    Acceptance rate: ${stats.acceptance_rate() * 100:.1f}%')
	println('')
}

fn demo_connection_migration() {
	println('Demo 3: Connection Migration')
	println('=' * 50)

	// Create initial connection
	local_addr := net.Addr{
		addr: '192.168.1.100:5000'
	}
	remote_addr := net.Addr{
		addr: '203.0.113.1:443'
	}

	mut migration := quic.new_connection_migration(local_addr, remote_addr)
	println('  ✓ Connection established')
	println('  Local address: ${local_addr.addr}')
	println('  Remote address: ${remote_addr.addr}')

	// Simulate network change (WiFi to cellular)
	println('\n  📱 Network change detected: WiFi → Cellular')
	new_local_addr := net.Addr{
		addr: '10.0.0.50:5001'
	}

	// Probe new path
	new_path := migration.probe_path(new_local_addr, remote_addr) or {
		println('  ❌ Failed to probe new path: ${err}')
		return
	}
	println('  ✓ New path probed: ${new_local_addr.addr}')
	println('  Migration state: ${migration.state}')
	println('  Alternative paths: ${migration.alternative_paths.len}')

	// Simulate path validation
	mut new_path_mut := new_path
	new_path_mut.validated = true
	new_path_mut.rtt = 50 * time.millisecond

	// Update in alternative paths
	if migration.alternative_paths.len > 0 {
		migration.alternative_paths[0].validated = true
		migration.alternative_paths[0].rtt = 50 * time.millisecond
	}

	println('  ✓ New path validated')
	println('  Path RTT: ${new_path_mut.rtt.milliseconds()} ms')

	// Migrate to new path
	migration.migrate_to_path(migration.alternative_paths[0]) or {
		println('  ❌ Migration failed: ${err}')
		return
	}
	println('  ✓ Successfully migrated to new path')
	println('  Current path: ${migration.current_path.local_addr.addr}')
	println('  Migration state: ${migration.state}')

	// Show migration statistics
	stats := migration.get_migration_stats()
	println('\n  Migration Statistics:')
	println('    Total migrations: ${stats.total_migrations}')
	println('    Successful: ${stats.successful_migrations}')
	println('    Success rate: ${stats.success_rate() * 100:.1f}%')
	println('    Network changes: ${stats.network_changes}')
	println('    NAT rebindings: ${stats.nat_rebindings}')

	// Demonstrate path degradation detection
	println('\n  🔍 Path Quality Monitoring:')

	// Good path
	is_degraded1 := migration.detect_path_degradation(0.01, 50 * time.millisecond)
	println('    Packet loss: 1%, RTT: 50ms → Degraded: ${is_degraded1}')

	// Degraded path (high packet loss)
	is_degraded2 := migration.detect_path_degradation(0.06, 50 * time.millisecond)
	println('    Packet loss: 6%, RTT: 50ms → Degraded: ${is_degraded2}')

	// Degraded path (high RTT)
	is_degraded3 := migration.detect_path_degradation(0.01, 600 * time.millisecond)
	println('    Packet loss: 1%, RTT: 600ms → Degraded: ${is_degraded3}')

	println('')
}
