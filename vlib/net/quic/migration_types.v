module quic

import net
import time

// Type definitions for QUIC connection migration.

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
	return cid.id[..cid.length] == other.id[..other.length]
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
