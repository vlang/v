module quic

import time

// RFC 9002 §5.3 — RTT estimation. QUIC's RTT estimator is deliberately
// per-CONNECTION, not per-packet-number-space (unlike packet numbers
// themselves, which ARE per-space -- see packet_number_space.v's own
// warning about not over-generalizing that independence to things that
// aren't actually independent). A single RttEstimator is shared across
// all three spaces; callers pass `space` into `update()` only so this file
// can apply the Initial/Handshake ACK-Delay-ignoring rule itself, not
// because the estimator's own state is split by space.

// kInitialRtt (RFC 9002 §5.3): the RTT estimate used before any real
// sample exists -- seeds smoothed_rtt so PTO computation (loss_detection.v)
// has a sane value even before a single packet has been acknowledged.
pub const initial_rtt = time.Duration(333 * time.millisecond)

// kGranularity (RFC 9002 §5.3): the system timer granularity assumed
// throughout loss detection -- both the time-threshold loss delay and the
// PTO calculation are floored at this value so an unrealistically small
// rttvar/smoothed_rtt can never produce a timer that fires more often than
// the assumed clock resolution.
pub const granularity = time.Duration(1 * time.millisecond)

// RttEstimator holds the one connection-wide RTT estimate. `min_rtt` and
// `has_sample` are meaningless before the first call to `update()`;
// `smoothed_rtt`/`rttvar` are pre-seeded to kInitialRtt/kInitialRtt/2 so a
// caller reading them before any sample still gets RFC 9002's specified
// pre-sample values rather than a misleading zero.
pub struct RttEstimator {
pub mut:
	min_rtt      time.Duration
	smoothed_rtt time.Duration
	rttvar       time.Duration
	latest_rtt   time.Duration
	has_sample   bool
}

pub fn new_rtt_estimator() RttEstimator {
	return RttEstimator{
		smoothed_rtt: initial_rtt
		rttvar:       initial_rtt / 2
	}
}

// update applies one new RTT sample (RFC 9002 §5.3's UpdateRtt). `space` is
// the packet number space the acknowledged, RTT-sampled packet was sent in
// -- `raw_ack_delay` (the ACK frame's own, not-yet-scaled ACK Delay field,
// already converted to a Duration by the caller via scaled_ack_delay_micros)
// is used ONLY for the application_data space; for Initial/Handshake it is
// unconditionally treated as zero (RFC 9002 §5.3: "An endpoint always
// ignores the ACK Delay field... for packets sent in the Initial and
// Handshake packet number space"). Deciding this HERE, from the `space`
// parameter, rather than trusting every caller to pre-zero the delay
// themselves, eliminates the trap of a caller forgetting the rule for one
// of the three spaces.
//
// `max_ack_delay` is the peer's own `max_ack_delay` transport parameter
// (RFC 9000 §18.2, in the same Duration units as everything else here);
// per RFC 9002 §5.3 the effective ack_delay is additionally clamped to this
// value, but ONLY once the handshake is confirmed -- `handshake_confirmed`
// mirrors handshake_confirm.v's own is_confirmed() checkpoint.
pub fn (mut r RttEstimator) update(space QuicPacketNumberSpace, latest_rtt time.Duration, raw_ack_delay time.Duration, max_ack_delay time.Duration, handshake_confirmed bool) {
	r.latest_rtt = latest_rtt

	if !r.has_sample {
		// First RTT sample: seed min_rtt/smoothed_rtt directly from the
		// sample and rttvar to half of it -- NOT the EWMA formula used for
		// every later sample (RFC 9002 §5.3 gives this as a distinct case;
		// applying the EWMA formula here would blend a real sample against
		// the kInitialRtt placeholder instead of replacing it outright).
		r.min_rtt = latest_rtt
		r.smoothed_rtt = latest_rtt
		r.rttvar = latest_rtt / 2
		r.has_sample = true
		return
	}

	// min_rtt ignores ack_delay entirely, in every space, always.
	if latest_rtt < r.min_rtt {
		r.min_rtt = latest_rtt
	}

	mut effective_ack_delay := if space == .application_data {
		raw_ack_delay
	} else {
		time.Duration(0)
	}
	if handshake_confirmed && effective_ack_delay > max_ack_delay {
		effective_ack_delay = max_ack_delay
	}

	// Only subtract the ack_delay if doing so is plausible -- i.e. it
	// cannot drive the adjusted sample below min_rtt (RFC 9002 §5.3: "if
	// latest_rtt >= min_rtt + ack_delay"). Otherwise the peer's reported
	// delay is untrustworthy for this sample and is not applied at all.
	mut adjusted_rtt := latest_rtt
	if latest_rtt >= r.min_rtt + effective_ack_delay {
		adjusted_rtt = latest_rtt - effective_ack_delay
	}

	diff := if r.smoothed_rtt > adjusted_rtt {
		r.smoothed_rtt - adjusted_rtt
	} else {
		adjusted_rtt - r.smoothed_rtt
	}
	r.rttvar = (r.rttvar * 3 + diff) / 4
	r.smoothed_rtt = (r.smoothed_rtt * 7 + adjusted_rtt) / 8
}

// pto_period returns the (smoothed_rtt + max(4*rttvar, kGranularity))
// component RFC 9002 §6.2.1's GetPtoTimeAndSpace formula shares across
// every space -- loss_detection.v scales this by 2^pto_count (and, for the
// application_data space only, adds max_ack_delay) itself.
pub fn (r &RttEstimator) pto_period() time.Duration {
	rttvar_term := 4 * r.rttvar
	floor := if rttvar_term > granularity { rttvar_term } else { granularity }
	return r.smoothed_rtt + floor
}
