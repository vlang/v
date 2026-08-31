module quic

// RFC 9000 §14 — Datagram Size / PMTU. v1 pins every datagram to the
// safe-minimum size (RFC 9000 §14.1: "the smallest maximum datagram size
// [that] MUST be supported" on any compliant path) rather than actively
// probing for a larger effective PMTU via DPLPMTUD (RFC 8899) -- a
// legitimate, spec-sanctioned conservative choice: "An endpoint that
// always sends datagrams of a size that fits within this limit MUST NOT
// probe for a larger PMTU". This trades extra throughput on paths that
// could support larger packets for zero additional complexity.
//
// This value already appears twice elsewhere for closely-related but
// distinct reasons -- `coalesce.v`'s `min_initial_datagram_size` (the
// Phase 4 rule that Initial-packet-carrying datagrams must be PADDED to
// at least this size) and `congestion_control.v`'s `max_datagram_size`
// (RFC 9002 Appendix B's per-packet byte accounting unit) -- this file
// reuses the latter as v1's pinned, non-probed effective PMTU rather than
// introducing a third constant for the same number.
//
// Connection migration (RFC 9000 §9) -- the other major consumer of
// accurate PMTU tracking, since a new path may have a different MTU --
// stays explicitly out of scope for v1 (see PROGRESS.md).

// fits_within_pmtu reports whether `datagram_len` is a legal size for
// this endpoint to send, given v1's pinned (non-probing) PMTU.
pub fn fits_within_pmtu(datagram_len int) bool {
	return datagram_len <= int(max_datagram_size)
}
