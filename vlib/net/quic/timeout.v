// timeout.v — idle timeout monitoring for QUIC connections (RFC 9000 §10.1).
module quic

import time

// IdleTimeoutMonitor tracks idle timeout state for a QUIC connection.
pub struct IdleTimeoutMonitor {
mut:
	idle_timeout_ms u64 // configured idle timeout in milliseconds
	last_activity   u64 // timestamp of last packet activity (ngtcp2 nanoseconds)
	expired         bool
}

// new_idle_timeout_monitor creates a new idle timeout monitor with the given timeout.
pub fn new_idle_timeout_monitor(timeout_ms u64) IdleTimeoutMonitor {
	return IdleTimeoutMonitor{
		idle_timeout_ms: timeout_ms
		last_activity:   u64(time.sys_mono_now())
		expired:         false
	}
}

// record_activity updates the last_activity timestamp to the current time.
// Per RFC 9000 §10.1, the idle timer restarts when a peer packet is processed.
pub fn (mut m IdleTimeoutMonitor) record_activity() {
	m.last_activity = u64(time.sys_mono_now())
}

// check_expired checks whether the idle timeout has elapsed since last activity.
// Returns true if expired; sets the expired flag. Returns false for nil connections.
pub fn (mut m IdleTimeoutMonitor) check_expired(mut conn Connection) bool {
	if conn.ngtcp2_conn == unsafe { nil } {
		return false
	}
	if m.expired {
		return true
	}
	now := u64(time.sys_mono_now())
	deadline := m.last_activity + m.idle_timeout_ms * 1000000
	if now >= deadline {
		m.expired = true
		return true
	}
	return false
}

// is_expired returns the current expired state.
pub fn (m &IdleTimeoutMonitor) is_expired() bool {
	return m.expired
}

// time_until_expiry returns milliseconds until idle expiry, or 0 if already expired.
pub fn (mut m IdleTimeoutMonitor) time_until_expiry(mut conn Connection) u64 {
	if m.expired {
		return 0
	}
	if conn.ngtcp2_conn == unsafe { nil } {
		return 0
	}
	now := u64(time.sys_mono_now())
	deadline := m.last_activity + m.idle_timeout_ms * 1000000
	if now >= deadline {
		return 0
	}
	return (deadline - now) / 1000000
}
