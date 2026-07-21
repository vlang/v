module quic

// RFC 9002 Appendix B — NewReno congestion control. Reacts purely to
// signals loss_detection.v already computed (newly-acked packets, lost
// packets, a persistent-congestion verdict) -- it holds no packet-tracking
// state of its own and never reads sent_packets directly, mirroring
// stream.v/flow_control.v's Phase 6 split (independently testable pieces a
// future connection wires together, Phase 9).
//
// v1 scope decision: app-limited detection (RFC 9002 §7.8's guidance to
// skip cwnd growth when the sender didn't have enough data to fully use
// the current window) is NOT implemented -- there is no real send queue to
// consult yet (that arrives with Phase 9's QuicConn). Omitting it is
// spec-legal (app-limited handling is explicitly optional guidance, not a
// MUST) and only means v1's cwnd can grow somewhat more eagerly than a
// fully app-limited-aware implementation would; it does not affect
// correctness of loss/recovery handling itself.

// kInitialWindow (RFC 9002 §7.2): min(10*max_datagram_size,
// max(2*max_datagram_size, 14720)) with max_datagram_size pinned to 1200
// (matching the safe-minimum PMTU Phase 4 already assumes for Initial
// packet padding) = min(12000, max(2400, 14720)) = min(12000, 14720) =
// 12000.
pub const max_datagram_size = u64(1200)
pub const initial_window = u64(12000)

// kMinimumWindow (RFC 9002 §7.2).
pub const minimum_window = u64(2 * max_datagram_size)

// kLossReductionFactor (RFC 9002 §7.2).
pub const loss_reduction_factor = 0.5

// NewRenoCongestionControl is RFC 9002 Appendix B.2's congestion controller
// state. `ssthresh` is none until the first congestion event (RFC 9002
// §7.2: "ssthresh is initialized to be unbounded"); is_in_slow_start()
// reports exactly that condition.
pub struct NewRenoCongestionControl {
pub mut:
	congestion_window              u64
	bytes_in_flight                u64
	ssthresh                       ?u64
	congestion_recovery_start_time ?u64
}

pub fn new_newreno_congestion_control() NewRenoCongestionControl {
	return NewRenoCongestionControl{
		congestion_window: initial_window
	}
}

// is_in_slow_start is RFC 9002 Appendix B.4's `congestion_window <
// ssthresh` check -- NOT simply "has a loss ever happened" (`ssthresh ==
// none`). Those two diverge after persistent congestion: on_packets_lost's
// persistent-congestion branch collapses congestion_window straight to
// kMinimumWindow while leaving the just-computed (larger) ssthresh
// untouched, so cwnd can legitimately fall back BELOW an already-set
// ssthresh -- at that point NewReno must re-enter slow start (full growth
// per ack) until cwnd climbs back up to ssthresh, not stay in congestion
// avoidance just because a loss happened at some earlier point.
pub fn (c &NewRenoCongestionControl) is_in_slow_start() bool {
	threshold := c.ssthresh or { return true }
	return c.congestion_window < threshold
}

// in_congestion_recovery is RFC 9002 Appendix B.7's InCongestionRecovery:
// a packet sent AT OR BEFORE the start of the current recovery period is
// considered part of the episode that triggered it, so it must not trigger
// a SECOND reaction (ssthresh halving) of its own -- this is what makes a
// burst of losses from one episode collapse the window exactly once
// instead of once per lost packet.
fn (c &NewRenoCongestionControl) in_congestion_recovery(sent_time u64) bool {
	start := c.congestion_recovery_start_time or { return false }
	return sent_time <= start
}

// on_packet_sent_cc is RFC 9002 Appendix B.3's OnPacketSentCC -- the
// caller (a future QuicConn) calls this itself for every packet
// loss_detection.v's on_packet_sent recorded with in_flight == true; the
// two are not automatically linked (see loss_detection.v's own on_packet_sent
// doc comment).
pub fn (mut c NewRenoCongestionControl) on_packet_sent_cc(sent_bytes u64) {
	c.bytes_in_flight += sent_bytes
}

fn (mut c NewRenoCongestionControl) remove_from_bytes_in_flight(sent_bytes u64) {
	if c.bytes_in_flight >= sent_bytes {
		c.bytes_in_flight -= sent_bytes
	} else {
		c.bytes_in_flight = 0
	}
}

// on_packets_acked is RFC 9002 Appendix B.4's OnPacketsAcked: grows the
// congestion window for every newly-acked, in-flight packet that is NOT
// part of the currently-ongoing recovery episode -- slow start grows by
// the full acked byte count, congestion avoidance grows proportionally
// (RFC 9002's standard AIMD increase).
pub fn (mut c NewRenoCongestionControl) on_packets_acked(acked_packets []SentPacketInfo) {
	for p in acked_packets {
		if p.in_flight {
			c.remove_from_bytes_in_flight(p.sent_bytes)
		}
		if !p.in_flight {
			continue
		}
		if c.in_congestion_recovery(p.time_sent) {
			continue
		}
		if c.is_in_slow_start() {
			c.congestion_window += p.sent_bytes
		} else {
			c.congestion_window += max_datagram_size * p.sent_bytes / c.congestion_window
		}
	}
}

// on_congestion_event is RFC 9002 Appendix B.5's OnCongestionEvent: the
// shared ssthresh-halving reaction used by both an ordinary loss and entry
// into persistent congestion. `sent_time` is the triggering packet's own
// send time (used only for the already-in-recovery check); `now` is the
// current time, which becomes the new recovery episode's start marker --
// these are deliberately different instants, not the same value.
fn (mut c NewRenoCongestionControl) on_congestion_event(sent_time u64, now u64) {
	if c.in_congestion_recovery(sent_time) {
		return
	}
	c.congestion_recovery_start_time = now
	new_ssthresh := u64(f64(c.congestion_window) * loss_reduction_factor)
	c.ssthresh = new_ssthresh
	c.congestion_window = if new_ssthresh > minimum_window { new_ssthresh } else { minimum_window }
}

// on_packets_lost is RFC 9002 Appendix B.6's OnPacketsLost: removes lost
// bytes from bytes_in_flight, reacts once via on_congestion_event (keyed
// off the LARGEST lost packet number's send time, per spec), and -- if
// `persistent_congestion` is true (loss_detection.v's is_persistent_congestion
// having already made that determination) -- collapses the window straight
// to kMinimumWindow and clears the recovery marker, a distinctly harsher
// reset than the ordinary ssthresh-halving path above, exercised only in
// this branch.
pub fn (mut c NewRenoCongestionControl) on_packets_lost(lost_packets []SentPacketInfo, persistent_congestion bool, now u64) {
	if lost_packets.len == 0 {
		return
	}
	for p in lost_packets {
		if p.in_flight {
			c.remove_from_bytes_in_flight(p.sent_bytes)
		}
	}

	mut largest_lost := lost_packets[0]
	for p in lost_packets {
		if p.packet_number > largest_lost.packet_number {
			largest_lost = p
		}
	}
	c.on_congestion_event(largest_lost.time_sent, now)

	if persistent_congestion {
		c.congestion_window = minimum_window
		c.congestion_recovery_start_time = none
	}
}
