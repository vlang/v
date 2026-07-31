module quic

import time

// RFC 9002 §6 — loss detection. This file implements the algorithmic core
// (RFC 9002 Appendix A's pseudocode) as a self-contained, directly
// unit-testable state machine, matching how packet_number_space.v and
// handshake_confirm.v were scoped in Phase 5/6: it does not itself send or
// retransmit anything -- an actual event loop wiring this to real packet
// transmission is Phase 9's QuicConn job. What this file DOES own is every
// decision an event loop will need: which packets are newly acked, which
// are lost, when the loss-detection/PTO timer should next fire, and which
// packet number space a PTO probe belongs in.

// kPacketThreshold (RFC 9002 §6.1.1).
pub const packet_threshold = u64(3)

// kTimeThreshold = 9/8 (RFC 9002 §6.1.2), applied as an integer
// multiply-then-divide rather than a float to avoid floating-point drift
// in a value that gates a security-relevant timer.
pub const time_threshold_numerator = i64(9)
pub const time_threshold_denominator = i64(8)

// kPersistentCongestionThreshold (RFC 9002 §7.6.2), in units of PTO.
pub const persistent_congestion_threshold = i64(3)

// SentPacketInfo is RFC 9002 Appendix A.1's per-packet sent_packets entry.
// `time_sent` is a time.sys_mono_now()-sourced monotonic nanosecond
// instant, not a wall-clock time (matching vlib/time's own StopWatch
// convention) -- QUIC's RTT/loss timers must never be perturbed by a
// system clock adjustment.
pub struct SentPacketInfo {
pub:
	packet_number    u64
	time_sent        u64
	sent_bytes       u64
	is_ack_eliciting bool
	in_flight        bool
}

// LossDetectionSpaceState is the per-packet-number-space slice of loss
// detection state (RFC 9002 Appendix A.1) -- a DIFFERENT decomposition than
// packet_number_space.v's PacketNumberSpaceState, which exists purely for
// packet-number encoding/decoding. Both happen to track "largest packet
// number the peer has acked in this space" because RFC 9002's own
// pseudocode structure keeps loss detection's copy separate from whatever
// an implementation's packet-number codec needs -- not a duplicated bug
// surface, since both are simple non-regressing trackers fed from the same
// ACK frame at the same time by whatever future glue code (Phase 9) reads
// one incoming ACK once.
pub struct LossDetectionSpaceState {
pub mut:
	sent_packets                      map[u64]SentPacketInfo
	largest_acked_packet              ?u64
	loss_time                         ?u64
	time_of_last_ack_eliciting_packet ?u64
}

// QuicLossDetectionTimer is the connection-wide loss detection state: three
// independent per-space slices (packet numbering IS independent per RFC
// 9000 §12.3 -- see packet_number_space.v), plus state that is
// deliberately NOT split by space: one shared RttEstimator (RFC 9002 §5.3
// treats RTT as a connection-wide property) and one shared pto_count. The
// PTO TIMER itself is also connection-wide, sourced from whichever space's
// own deadline is earliest (pto_time_and_space) -- conflating "packet
// numbers are per-space" with "therefore everything must be per-space"
// would be its own distinct bug, flagged explicitly because it's the
// opposite mistake from the one packet_number_space.v warns about.
pub struct QuicLossDetectionTimer {
pub mut:
	initial          LossDetectionSpaceState
	handshake        LossDetectionSpaceState
	application_data LossDetectionSpaceState
	rtt              RttEstimator
	pto_count        int
}

pub fn new_quic_loss_detection_timer() &QuicLossDetectionTimer {
	return &QuicLossDetectionTimer{
		rtt: new_rtt_estimator()
	}
}

fn (mut ld QuicLossDetectionTimer) space_state(space QuicPacketNumberSpace) &LossDetectionSpaceState {
	match space {
		.initial { return &ld.initial }
		.handshake { return &ld.handshake }
		.application_data { return &ld.application_data }
	}
}

fn has_in_flight_ack_eliciting(st &LossDetectionSpaceState) bool {
	for _, info in st.sent_packets {
		if info.is_ack_eliciting && info.in_flight {
			return true
		}
	}
	return false
}

fn ack_ranges_cover(ranges []AckRange, pn u64) bool {
	for r in ranges {
		if pn >= r.smallest && pn <= r.largest {
			return true
		}
	}
	return false
}

// on_packet_sent is RFC 9002 Appendix A.5's OnPacketSent -- must be called
// for EVERY packet sent in this space, not only ack-eliciting ones: a
// peer's ACK frame can legally reference the packet number of a packet
// that wasn't itself ack-eliciting (e.g. one carrying only an ACK frame of
// our own), and that packet number still needs a sent_packets entry to be
// removable by on_ack_received. Congestion-control bookkeeping
// (OnPacketSentCC) is deliberately NOT called from here -- that's the
// caller's job once it also holds a NewRenoCongestionControl (Phase 9),
// mirroring flow_control.v's own "keep this decoupled from owning that
// state" precedent.
pub fn (mut ld QuicLossDetectionTimer) on_packet_sent(space QuicPacketNumberSpace, packet_number u64, sent_bytes u64, is_ack_eliciting bool, in_flight bool, now u64) {
	mut st := ld.space_state(space)
	st.sent_packets[packet_number] = SentPacketInfo{
		packet_number:    packet_number
		time_sent:        now
		sent_bytes:       sent_bytes
		is_ack_eliciting: is_ack_eliciting
		in_flight:        in_flight
	}
	if is_ack_eliciting {
		st.time_of_last_ack_eliciting_packet = now
	}
}

// AckProcessingResult reports what one on_ack_received call discovered:
// which previously-sent packets it newly acknowledged, which packets it
// caused to be declared lost, and whether that loss batch meets RFC
// 9002 §7.6.2's persistent-congestion condition.
pub struct AckProcessingResult {
pub:
	newly_acked           []SentPacketInfo
	lost                  []SentPacketInfo
	persistent_congestion bool
}

// on_ack_received is RFC 9002 Appendix A.6's OnAckReceived. `ack_delay_exponent`
// is the PEER's own ack_delay_exponent transport parameter (default 3,
// frame.v's default_ack_delay_exponent), used to scale the ACK frame's raw
// ack_delay field before RTT sampling. `max_ack_delay` is the peer's
// max_ack_delay transport parameter, applied per RttEstimator.update's own
// handshake-confirmed-gated clamp.
pub fn (mut ld QuicLossDetectionTimer) on_ack_received(space QuicPacketNumberSpace, ack AckFrame, ack_delay_exponent u64, max_ack_delay time.Duration, handshake_confirmed bool, now u64) AckProcessingResult {
	mut st := ld.space_state(space)

	if existing := st.largest_acked_packet {
		if ack.largest_acknowledged > existing {
			st.largest_acked_packet = ack.largest_acknowledged
		}
	} else {
		st.largest_acked_packet = ack.largest_acknowledged
	}

	// Iterate OUR OWN sent_packets (bounded by how many packets we
	// actually have outstanding) and test each against the ack's ranges,
	// rather than iterating the ranges' own [smallest, largest] spans
	// directly. The latter would iterate a peer-controlled span: a single
	// ACK frame's largest_acknowledged/first_ack_range are independent
	// varints (RFC 9000 §19.3), so a tiny, well-formed frame can legally
	// claim a range spanning up to 2^62-1 packet numbers with no
	// relationship to the wire size -- iterating that directly is an
	// unbounded-CPU DoS handed to a peer that supplies just two varints.
	mut newly_acked := []SentPacketInfo{}
	mut acked_pns := []u64{}
	for pn, info in st.sent_packets {
		if ack_ranges_cover(ack.ranges, pn) {
			newly_acked << info
			acked_pns << pn
		}
	}
	for pn in acked_pns {
		st.sent_packets.delete(pn)
	}

	if newly_acked.len == 0 {
		return AckProcessingResult{}
	}

	// RTT sample: only if the ACK's own largest_acknowledged was ITSELF
	// newly acked here (not merely covered by an earlier, already-processed
	// ACK) AND at least one newly-acked packet was ack-eliciting (RFC 9002
	// §5.3 / Appendix A.6).
	mut has_ack_eliciting := false
	mut found_largest := false
	mut largest_newly_acked := SentPacketInfo{}
	for info in newly_acked {
		if info.is_ack_eliciting {
			has_ack_eliciting = true
		}
		if info.packet_number == ack.largest_acknowledged {
			largest_newly_acked = info
			found_largest = true
		}
	}
	if found_largest && has_ack_eliciting {
		latest_rtt := time.Duration(i64(now) - i64(largest_newly_acked.time_sent))
		raw_ack_delay_micros := scaled_ack_delay_micros(ack.ack_delay, ack_delay_exponent)
		raw_ack_delay := time.Duration(i64(raw_ack_delay_micros) * i64(time.microsecond))
		ld.rtt.update(space, latest_rtt, raw_ack_delay, max_ack_delay, handshake_confirmed)
	}

	lost := ld.detect_and_remove_lost_packets(space, now)

	mut persistent_congestion := false
	if lost.len > 0 {
		persistent_congestion = is_persistent_congestion(lost, ld.rtt.pto_period())
	} else {
		// "Reset PTO count unless a packet is lost" (RFC 9002 Appendix A.6)
		// -- deliberately NOT reset when something WAS lost; only an actual
		// PTO firing (on_loss_detection_timeout) increments it, so an
		// ordinary ack-triggered loss must leave it untouched, not reset it.
		ld.pto_count = 0
	}

	return AckProcessingResult{
		newly_acked:           newly_acked
		lost:                  lost
		persistent_congestion: persistent_congestion
	}
}

// detect_and_remove_lost_packets is RFC 9002 Appendix A.10's
// DetectAndRemoveLostPackets: a packet in this space is declared lost if
// EITHER the packet-threshold (largest_acked_packet is at least
// kPacketThreshold higher) OR the time-threshold (sent at least
// 9/8*max(latest_rtt,smoothed_rtt), floored at kGranularity, before now) is
// met -- either condition alone is sufficient, this is not an AND of both.
// Must only be called once largest_acked_packet is known for this space
// (an ACK covering something in it must have been processed first);
// returns empty otherwise.
pub fn (mut ld QuicLossDetectionTimer) detect_and_remove_lost_packets(space QuicPacketNumberSpace, now u64) []SentPacketInfo {
	largest_acked := ld.space_state(space).largest_acked_packet or { return []SentPacketInfo{} }

	loss_delay_rtt := if ld.rtt.latest_rtt > ld.rtt.smoothed_rtt {
		ld.rtt.latest_rtt
	} else {
		ld.rtt.smoothed_rtt
	}
	mut loss_delay := time.Duration(i64(loss_delay_rtt) * time_threshold_numerator / time_threshold_denominator)
	if loss_delay < granularity {
		loss_delay = granularity
	}
	lost_send_time_threshold := i64(now) - i64(loss_delay)

	mut st := ld.space_state(space)
	st.loss_time = none

	mut lost := []SentPacketInfo{}
	mut to_remove := []u64{}
	for pn, info in st.sent_packets {
		if pn > largest_acked {
			continue
		}
		if i64(info.time_sent) <= lost_send_time_threshold || largest_acked >= pn + packet_threshold {
			to_remove << pn
			if info.in_flight {
				lost << info
			}
		} else {
			candidate := info.time_sent + u64(loss_delay)
			if existing := st.loss_time {
				if candidate < existing {
					st.loss_time = candidate
				}
			} else {
				st.loss_time = candidate
			}
		}
	}
	for pn in to_remove {
		st.sent_packets.delete(pn)
	}
	return lost
}

// loss_time_and_space is RFC 9002 Appendix A.8's GetLossTimeAndSpace:
// whichever space's own loss_time is earliest, across all three
// independent spaces.
pub fn (mut ld QuicLossDetectionTimer) loss_time_and_space() ?(u64, QuicPacketNumberSpace) {
	spaces := [QuicPacketNumberSpace.initial, QuicPacketNumberSpace.handshake,
		QuicPacketNumberSpace.application_data]

	mut best_time := u64(0)
	mut best_space := QuicPacketNumberSpace.initial
	mut found := false
	for sp in spaces {
		lt := ld.space_state(sp).loss_time or { continue }
		if !found || lt < best_time {
			best_time = lt
			best_space = sp
			found = true
		}
	}
	if !found {
		return none
	}
	return best_time, best_space
}

// pto_time_and_space is RFC 9002 Appendix A.8's GetPtoTimeAndSpace: when
// the SINGLE connection-wide PTO timer should next fire, and which space it
// will probe. A space with no ack-eliciting packet currently in flight
// contributes no candidate deadline at all (there is nothing there to
// probe for); the application_data space additionally contributes nothing
// until the handshake is confirmed (RFC 9001 -- 1-RTT keys/PTO only matter
// once that space is actually in use for anything the peer must respond
// to), at which point its own contribution also adds max_ack_delay (scaled
// by the same 2^pto_count backoff) since a 1-RTT ACK may legitimately be
// delayed by up to that much.
pub fn (mut ld QuicLossDetectionTimer) pto_time_and_space(handshake_confirmed bool, max_ack_delay time.Duration) (u64, QuicPacketNumberSpace) {
	backoff := u64(1) << u32(ld.pto_count)
	base_duration := i64(ld.rtt.pto_period()) * i64(backoff)

	spaces := [QuicPacketNumberSpace.initial, QuicPacketNumberSpace.handshake,
		QuicPacketNumberSpace.application_data]

	mut pto_timeout := u64(0)
	mut have_timeout := false
	mut pto_space := QuicPacketNumberSpace.initial

	for sp in spaces {
		st := ld.space_state(sp)
		if !has_in_flight_ack_eliciting(st) {
			continue
		}
		mut duration := base_duration
		if sp == .application_data {
			if !handshake_confirmed {
				break
			}
			duration += i64(max_ack_delay) * i64(backoff)
		}
		last := st.time_of_last_ack_eliciting_packet or { continue }
		t := last + u64(duration)
		if !have_timeout || t < pto_timeout {
			pto_timeout = t
			pto_space = sp
			have_timeout = true
		}
	}
	return pto_timeout, pto_space
}

// next_timeout is RFC 9002 Appendix A.7's SetLossDetectionTimer, scoped to
// v1's client-only reality: the anti-amplification-limited branch in the
// spec's pseudocode is server-only (it concerns how much a SERVER may send
// before validating the client's address) and never applies here. Returns
// none when the loss-detection timer should be cancelled entirely --
// nothing ack-eliciting is outstanding anywhere, so there is nothing to
// time out.
pub fn (mut ld QuicLossDetectionTimer) next_timeout(handshake_confirmed bool, max_ack_delay time.Duration, bytes_in_flight u64) ?(u64, QuicPacketNumberSpace) {
	if loss_time, loss_space := ld.loss_time_and_space() {
		return loss_time, loss_space
	}
	if bytes_in_flight == 0 {
		return none
	}
	t, sp := ld.pto_time_and_space(handshake_confirmed, max_ack_delay)
	return t, sp
}

// LossTimeoutResult reports what firing the loss-detection timer found:
// EITHER a time-threshold loss batch (pto_fired == false, lost non-empty)
// OR a genuine PTO expiry (pto_fired == true) naming the space a probe
// belongs in -- RFC 9002 Appendix A.9's OnLossDetectionTimeout always
// re-checks loss_time_and_space FIRST, since the timer may have been armed
// for a loss deadline that a race already resolved differently by the time
// it fires.
pub struct LossTimeoutResult {
pub:
	lost      []SentPacketInfo
	pto_fired bool
	pto_space QuicPacketNumberSpace
}

pub fn (mut ld QuicLossDetectionTimer) on_loss_detection_timeout(now u64, handshake_confirmed bool, max_ack_delay time.Duration) LossTimeoutResult {
	if _, loss_space := ld.loss_time_and_space() {
		lost := ld.detect_and_remove_lost_packets(loss_space, now)
		return LossTimeoutResult{
			lost: lost
		}
	}

	_, pto_space := ld.pto_time_and_space(handshake_confirmed, max_ack_delay)
	ld.pto_count++
	return LossTimeoutResult{
		pto_fired: true
		pto_space: pto_space
	}
}

// is_persistent_congestion determines RFC 9002 §7.6.2's persistent
// congestion collapse condition from a SINGLE detect_and_remove_lost_packets
// batch: two ack-eliciting lost packets spanning at least
// kPersistentCongestionThreshold PTOs apart, with every packet number in
// between also present in this same lost batch (nothing sent between them
// survived as acked or still-outstanding).
//
// Scope note: this checks contiguity only WITHIN one detection pass rather
// than stitching together multiple separate loss-detection calls. A
// genuine persistent-congestion episode -- an extended stall with no acks
// at all -- is realistically detected by exactly one PTO-triggered
// time-threshold pass covering the whole stalled range (nothing intervenes
// to fragment it, since nothing is being acked), so this matches the
// real-world trigger pattern. A contrived scenario spanning multiple
// separate detect-lost calls with no intervening acks is not merged in
// v1; failing to collapse in that narrow case is the conservative
// direction to err in (never wrongly collapsing the window), not an
// incorrect one.
pub fn is_persistent_congestion(lost_packets []SentPacketInfo, pto time.Duration) bool {
	mut ack_eliciting := lost_packets.filter(it.is_ack_eliciting)
	if ack_eliciting.len < 2 {
		return false
	}
	ack_eliciting.sort(a.packet_number < b.packet_number)

	earliest := ack_eliciting[0]
	latest := ack_eliciting[ack_eliciting.len - 1]

	congestion_period := time.Duration(i64(pto) * persistent_congestion_threshold)
	span := time.Duration(i64(latest.time_sent) - i64(earliest.time_sent))
	if span < congestion_period {
		return false
	}

	mut present := map[u64]bool{}
	for p in lost_packets {
		present[p.packet_number] = true
	}
	mut pn := earliest.packet_number
	for pn <= latest.packet_number {
		if pn !in present {
			return false
		}
		pn++
	}
	return true
}
