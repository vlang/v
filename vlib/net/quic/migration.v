// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module quic

import net
import time

// Connection Migration allows QUIC connections to survive network changes
// (e.g., WiFi to cellular, IP address changes)

// ConnectionID represents a QUIC connection ID
pub struct ConnectionID {
pub:
	id     []u8
	length u8
}

pub fn new_connection_id(id []u8) ConnectionID {
	return ConnectionID{
		id:     id.clone()
		length: u8(id.len)
	}
}

pub fn (cid &ConnectionID) equals(other ConnectionID) bool {
	if cid.length != other.length {
		return false
	}
	// Use bulk comparison instead of byte-by-byte
	if cid.length == 0 {
		return true
	}
	unsafe {
		return C.memcmp(cid.id.data, other.id.data, cid.length) == 0
	}
}

pub fn (cid &ConnectionID) str() string {
	return cid.id.hex()
}

// PathInfo represents network path information
pub struct PathInfo {
pub mut:
	local_addr  net.Addr
	remote_addr net.Addr
	rtt         time.Duration
	validated   bool
	active      bool
	mtu         u16 = 1200 // Default minimum MTU
}

pub fn new_path_info(local_addr net.Addr, remote_addr net.Addr) PathInfo {
	return PathInfo{
		local_addr:  local_addr
		remote_addr: remote_addr
		rtt:         time.Duration(0)
		validated:   false
		active:      false
	}
}

// MigrationState represents the state of connection migration
pub enum MigrationState {
	idle
	probing    // Probing new path
	validating // Validating new path
	migrating  // Switching to new path
	completed  // Migration completed
	failed     // Migration failed
}

// MigrationReason indicates why migration was triggered
pub enum MigrationReason {
	network_change   // Network interface changed
	nat_rebinding    // NAT rebinding detected
	path_degradation // Current path quality degraded
	manual           // Manual migration request
	peer_migration   // Peer initiated migration
}

// PathChallenge represents a PATH_CHALLENGE frame
struct PathChallenge {
	data [8]u8
}

// PathResponse represents a PATH_RESPONSE frame
struct PathResponse {
	data [8]u8
}

// MigrationEvent represents a migration event
pub struct MigrationEvent {
pub:
	reason    MigrationReason
	old_path  PathInfo
	new_path  PathInfo
	timestamp time.Time
	success   bool
}

// ConnectionMigration manages connection migration
pub struct ConnectionMigration {
pub mut:
	enabled            bool = true
	current_path       PathInfo
	alternative_paths  []PathInfo
	state              MigrationState
	pending_challenges map[string]PathChallenge
	migration_history  []MigrationEvent
	max_paths          int           = 4
	probe_timeout      time.Duration = 3 * time.second
}

pub fn new_connection_migration(local_addr net.Addr, remote_addr net.Addr) ConnectionMigration {
	return ConnectionMigration{
		enabled:            true
		current_path:       new_path_info(local_addr, remote_addr)
		alternative_paths:  []PathInfo{}
		state:              .idle
		pending_challenges: map[string]PathChallenge{}
		migration_history:  []MigrationEvent{}
	}
}

// probe_path initiates path validation for a new path
pub fn (mut cm ConnectionMigration) probe_path(local_addr net.Addr, remote_addr net.Addr) !PathInfo {
	if !cm.enabled {
		return error('Connection migration is disabled')
	}

	if cm.alternative_paths.len >= cm.max_paths {
		return error('Maximum number of paths reached')
	}

	// Create new path
	mut new_path := new_path_info(local_addr, remote_addr)

	// Generate PATH_CHALLENGE
	challenge := generate_path_challenge()
	path_key := path_to_key(new_path)
	cm.pending_challenges[path_key] = challenge

	// Add to alternative paths
	cm.alternative_paths << new_path
	cm.state = .probing

	return new_path
}

// validate_path validates a path using PATH_RESPONSE
pub fn (mut cm ConnectionMigration) validate_path(path PathInfo, response PathResponse) !bool {
	path_key := path_to_key(path)

	if challenge := cm.pending_challenges[path_key] {
		// Verify response matches challenge
		if response.data == challenge.data {
			// Mark path as validated
			for mut alt_path in cm.alternative_paths {
				if paths_equal(alt_path, path) {
					alt_path.validated = true
					cm.state = .validating
					return true
				}
			}
		}
	}

	return false
}

// migrate_to_path switches to a new validated path
pub fn (mut cm ConnectionMigration) migrate_to_path(new_path PathInfo) !bool {
	if !new_path.validated {
		return error('Cannot migrate to unvalidated path')
	}

	old_path := cm.current_path

	// Switch to new path
	cm.current_path = new_path
	cm.current_path.active = true
	cm.state = .migrating

	// Record migration event
	event := MigrationEvent{
		reason:    .manual
		old_path:  old_path
		new_path:  new_path
		timestamp: time.now()
		success:   true
	}
	cm.migration_history << event

	// Remove from alternative paths
	cm.alternative_paths = cm.alternative_paths.filter(!paths_equal(it, new_path))

	cm.state = .completed

	return true
}

// handle_network_change handles network interface changes
pub fn (mut cm ConnectionMigration) handle_network_change(new_local_addr net.Addr) ! {
	if !cm.enabled {
		return
	}

	// Check if local address actually changed
	if addrs_equal(new_local_addr, cm.current_path.local_addr) {
		return
	}

	// Probe new path
	new_path := cm.probe_path(new_local_addr, cm.current_path.remote_addr)!

	// Record event
	event := MigrationEvent{
		reason:    .network_change
		old_path:  cm.current_path
		new_path:  new_path
		timestamp: time.now()
		success:   false // Will be updated when migration completes
	}
	cm.migration_history << event
}

// handle_nat_rebinding handles NAT rebinding
pub fn (mut cm ConnectionMigration) handle_nat_rebinding(new_remote_addr net.Addr) ! {
	if !cm.enabled {
		return
	}

	// Update current path with new remote address
	old_path := cm.current_path
	cm.current_path.remote_addr = new_remote_addr

	// Record event
	event := MigrationEvent{
		reason:    .nat_rebinding
		old_path:  old_path
		new_path:  cm.current_path
		timestamp: time.now()
		success:   true
	}
	cm.migration_history << event
}

// detect_path_degradation checks if current path quality has degraded
pub fn (cm &ConnectionMigration) detect_path_degradation(packet_loss_rate f64, rtt time.Duration) bool {
	// Simple heuristics for path degradation
	high_loss := packet_loss_rate > 0.05 // 5% packet loss
	high_rtt := rtt > 500 * time.millisecond

	return high_loss || high_rtt
}

// select_best_path selects the best alternative path
pub fn (cm &ConnectionMigration) select_best_path() ?PathInfo {
	mut best_path := ?PathInfo(none)
	mut best_rtt := time.Duration(i64(1) << 62) // Max duration

	for path in cm.alternative_paths {
		if path.validated && path.rtt < best_rtt {
			best_path = path
			best_rtt = path.rtt
		}
	}

	return best_path
}

// cleanup_paths removes invalid or old paths
pub fn (mut cm ConnectionMigration) cleanup_paths() {
	now := time.now()

	// Remove unvalidated paths older than probe timeout
	cm.alternative_paths = cm.alternative_paths.filter(it.validated
		|| (now - time.Time{}).seconds() < cm.probe_timeout.seconds())

	// Clean up pending challenges
	mut to_remove := []string{}
	for key, _ in cm.pending_challenges {
		// Remove challenges older than timeout
		to_remove << key
	}

	for key in to_remove {
		cm.pending_challenges.delete(key)
	}
}

// get_migration_stats returns migration statistics
pub fn (cm &ConnectionMigration) get_migration_stats() MigrationStats {
	mut stats := MigrationStats{}

	for event in cm.migration_history {
		stats.total_migrations++
		if event.success {
			stats.successful_migrations++
		}

		match event.reason {
			.network_change { stats.network_changes++ }
			.nat_rebinding { stats.nat_rebindings++ }
			.path_degradation { stats.path_degradations++ }
			.manual { stats.manual_migrations++ }
			.peer_migration { stats.peer_migrations++ }
		}
	}

	return stats
}

// MigrationStats tracks migration statistics
pub struct MigrationStats {
pub mut:
	total_migrations      u64
	successful_migrations u64
	network_changes       u64
	nat_rebindings        u64
	path_degradations     u64
	manual_migrations     u64
	peer_migrations       u64
}

pub fn (stats &MigrationStats) success_rate() f64 {
	if stats.total_migrations == 0 {
		return 0.0
	}
	return f64(stats.successful_migrations) / f64(stats.total_migrations)
}

// Helper functions

fn generate_path_challenge() PathChallenge {
	// In production, use cryptographically secure random bytes
	mut data := [8]u8{}
	for i in 0 .. 8 {
		data[i] = u8(i * 17) // Simple pattern for now
	}
	return PathChallenge{
		data: data
	}
}

fn path_to_key(path PathInfo) string {
	return '${path.local_addr}:${path.remote_addr}'
}

fn paths_equal(p1 PathInfo, p2 PathInfo) bool {
	return addrs_equal(p1.local_addr, p2.local_addr) && addrs_equal(p1.remote_addr, p2.remote_addr)
}

fn addrs_equal(a1 net.Addr, a2 net.Addr) bool {
	return a1.str() == a2.str()
}

// MigrationPolicy defines when to trigger migration
pub struct MigrationPolicy {
pub:
	auto_migrate_on_network_change bool          = true
	auto_migrate_on_degradation    bool          = true
	packet_loss_threshold          f64           = 0.05 // 5%
	rtt_threshold                  time.Duration = 500 * time.millisecond
	probe_interval                 time.Duration = 30 * time.second
}

// MigrationController manages migration policy and decisions
pub struct MigrationController {
pub mut:
	migration ConnectionMigration
	policy    MigrationPolicy
	stats     MigrationStats
}

pub fn new_migration_controller(local_addr net.Addr, remote_addr net.Addr, policy MigrationPolicy) MigrationController {
	return MigrationController{
		migration: new_connection_migration(local_addr, remote_addr)
		policy:    policy
		stats:     MigrationStats{}
	}
}

// evaluate evaluates whether migration should be triggered
pub fn (mut mc MigrationController) evaluate(packet_loss_rate f64, rtt time.Duration) !bool {
	if !mc.policy.auto_migrate_on_degradation {
		return false
	}

	if mc.migration.detect_path_degradation(packet_loss_rate, rtt) {
		// Try to find better path
		if best_path := mc.migration.select_best_path() {
			mc.migration.migrate_to_path(best_path)!
			mc.stats.successful_migrations++
			return true
		}
	}

	return false
}
