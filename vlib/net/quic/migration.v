module quic

import crypto.rand
import net
import sync
import time

// QUIC connection migration for surviving network path changes.

// ConnectionID represents a QUIC connection ID
pub struct ConnectionID {
pub:
	id     []u8
	length u8
}

// new_connection_id creates a new ConnectionID from the given byte slice.
pub fn new_connection_id(id []u8) ConnectionID {
	return ConnectionID{
		id:     id.clone()
		length: u8(id.len)
	}
}

// equals checks if two ConnectionIDs are equal.
pub fn (cid &ConnectionID) equals(other ConnectionID) bool {
	if cid.length != other.length {
		return false
	}
	if cid.length == 0 {
		return true
	}
	unsafe {
		return C.memcmp(cid.id.data, other.id.data, cid.length) == 0
	}
}

// str returns the hexadecimal string representation of the ConnectionID.
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
	created_at  time.Time // Time when this path was created, used for cleanup timeout
}

// new_path_info creates a new PathInfo for the given local and remote addresses.
pub fn new_path_info(local_addr net.Addr, remote_addr net.Addr) PathInfo {
	return PathInfo{
		local_addr:  local_addr
		remote_addr: remote_addr
		rtt:         time.Duration(0)
		validated:   false
		active:      false
		created_at:  time.now()
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
pub struct PathChallenge {
pub:
	data [8]u8
}

// PathResponse represents a PATH_RESPONSE frame
pub struct PathResponse {
pub:
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
mut:
	mu &sync.Mutex = sync.new_mutex()
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

// new_connection_migration creates a new ConnectionMigration manager for the given addresses.
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

// probe_path initiates path validation for a new path (thread-safe)
pub fn (mut cm ConnectionMigration) probe_path(local_addr net.Addr, remote_addr net.Addr) !PathInfo {
	cm.mu.lock()
	if !cm.enabled {
		cm.mu.unlock()
		return error('Connection migration is disabled')
	}

	if cm.alternative_paths.len >= cm.max_paths {
		cm.mu.unlock()
		return error('Maximum number of paths reached')
	}

	mut new_path := new_path_info(local_addr, remote_addr)

	challenge := generate_path_challenge() or {
		cm.mu.unlock()
		return error('failed to generate path challenge: ${err}')
	}
	path_key := path_to_key(new_path)
	cm.pending_challenges[path_key] = challenge

	cm.alternative_paths << new_path
	cm.state = .probing

	cm.mu.unlock()
	return new_path
}

// validate_path validates a path using PATH_RESPONSE (thread-safe)
pub fn (mut cm ConnectionMigration) validate_path(path PathInfo, response PathResponse) !bool {
	cm.mu.lock()
	path_key := path_to_key(path)

	if challenge := cm.pending_challenges[path_key] {
		if response.data == challenge.data {
			for mut alt_path in cm.alternative_paths {
				if paths_equal(alt_path, path) {
					alt_path.validated = true
					cm.state = .validating
					cm.mu.unlock()
					return true
				}
			}
		}
	}

	cm.mu.unlock()
	return false
}

// migrate_to_path switches to a new validated path (thread-safe)
pub fn (mut cm ConnectionMigration) migrate_to_path(new_path PathInfo) !bool {
	cm.mu.lock()
	if !new_path.validated {
		cm.mu.unlock()
		return error('Cannot migrate to unvalidated path')
	}

	old_path := cm.current_path

	cm.current_path = new_path
	cm.current_path.active = true
	cm.state = .migrating

	event := MigrationEvent{
		reason:    .manual
		old_path:  old_path
		new_path:  new_path
		timestamp: time.now()
		success:   true
	}
	cm.migration_history << event

	cm.alternative_paths = cm.alternative_paths.filter(!paths_equal(it, new_path))

	cm.state = .completed

	cm.mu.unlock()
	return true
}

// handle_network_change handles network interface changes
pub fn (mut cm ConnectionMigration) handle_network_change(new_local_addr net.Addr) ! {
	if !cm.enabled {
		return
	}

	if addrs_equal(new_local_addr, cm.current_path.local_addr) {
		return
	}

	new_path := cm.probe_path(new_local_addr, cm.current_path.remote_addr)!

	event := MigrationEvent{
		reason:    .network_change
		old_path:  cm.current_path
		new_path:  new_path
		timestamp: time.now()
		success:   false // Will be updated when migration completes
	}
	cm.migration_history << event
}

// handle_nat_rebinding handles NAT rebinding (thread-safe)
pub fn (mut cm ConnectionMigration) handle_nat_rebinding(new_remote_addr net.Addr) ! {
	cm.mu.lock()
	if !cm.enabled {
		cm.mu.unlock()
		return
	}

	old_path := cm.current_path
	cm.current_path.remote_addr = new_remote_addr

	event := MigrationEvent{
		reason:    .nat_rebinding
		old_path:  old_path
		new_path:  cm.current_path
		timestamp: time.now()
		success:   true
	}
	cm.migration_history << event
	cm.mu.unlock()
}

// detect_path_degradation checks if current path quality has degraded
pub fn (cm &ConnectionMigration) detect_path_degradation(packet_loss_rate f64, rtt time.Duration) bool {
	high_loss := packet_loss_rate > 0.05
	high_rtt := rtt > 500 * time.millisecond

	return high_loss || high_rtt
}

// select_best_path selects the best alternative path
pub fn (cm &ConnectionMigration) select_best_path() ?PathInfo {
	mut best_path := ?PathInfo(none)
	mut best_rtt := time.Duration(i64(u64(1) << 62)) // Max duration

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
	cm.alternative_paths = cm.alternative_paths.filter(it.validated
		|| time.since(it.created_at) < cm.probe_timeout)

	mut to_remove := []string{}
	for key, _ in cm.pending_challenges {
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

fn generate_path_challenge() !PathChallenge {
	// RFC 9000 §8.2.1: PATH_CHALLENGE data must be 8 cryptographically random bytes.
	random_bytes := rand.read(8) or {
		return error('failed to generate PATH_CHALLENGE: RNG failure — ${err}')
	}
	mut data := [8]u8{}
	for i in 0 .. 8 {
		data[i] = random_bytes[i]
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
