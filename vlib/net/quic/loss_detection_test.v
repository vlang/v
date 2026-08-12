// vtest build: present_openssl?
module quic

import time

fn test_on_packet_sent_records_time_of_last_ack_eliciting_packet() {
	mut ld := new_quic_loss_detection_timer()
	ld.on_packet_sent(.initial, 0, 1200, false, false, 500)
	assert ld.initial.time_of_last_ack_eliciting_packet == none

	ld.on_packet_sent(.initial, 1, 1200, true, true, 700)
	assert ld.initial.time_of_last_ack_eliciting_packet or { 0 } == 700
	assert ld.initial.sent_packets.len == 2
}

// test_on_ack_received_tolerates_astronomically_large_ack_range is a
// regression test for a real DoS: an ACK frame's largest_acknowledged and
// first_ack_range are independent varints (RFC 9000 §19.3), so a tiny,
// well-formed frame can legally claim a range spanning up to 2^62-1 packet
// numbers with no relationship to the wire size. on_ack_received must
// iterate its OWN bounded sent_packets, never the range's raw span, or a
// peer can hang this call with a handful of bytes.
fn test_on_ack_received_tolerates_astronomically_large_ack_range() {
	mut ld := new_quic_loss_detection_timer()
	ld.on_packet_sent(.application_data, 5, 1200, true, true, 1000)
	ld.on_packet_sent(.application_data, 9, 1200, true, true, 1000)

	ack := AckFrame{
		largest_acknowledged: u64(1) << 62
		ack_delay:            0
		ranges:               [AckRange{
			smallest: 0
			largest:  u64(1) << 62
		}]
	}
	result := ld.on_ack_received(.application_data, ack, default_ack_delay_exponent,
		25 * time.millisecond, false, 2000)

	// Both actually-sent packets fall inside the huge range and are
	// correctly newly-acked; nothing else is fabricated.
	assert result.newly_acked.len == 2
	assert ld.application_data.sent_packets.len == 0
}

fn test_on_ack_received_marks_newly_acked_and_removes_from_sent_packets() {
	mut ld := new_quic_loss_detection_timer()
	ld.on_packet_sent(.application_data, 0, 1200, true, true, 1000)
	ld.on_packet_sent(.application_data, 1, 1200, true, true, 1000)
	ld.on_packet_sent(.application_data, 2, 1200, true, true, 1000)

	ack := AckFrame{
		largest_acknowledged: 2
		ack_delay:            0
		ranges:               [AckRange{
			smallest: 0
			largest:  2
		}]
	}
	result := ld.on_ack_received(.application_data, ack, default_ack_delay_exponent,
		25 * time.millisecond, false, 2000)

	assert result.newly_acked.len == 3
	assert result.lost.len == 0
	assert ld.application_data.sent_packets.len == 0
	// The largest_acknowledged (pn 2) was itself newly acked and
	// ack-eliciting, so an RTT sample must have been taken: 2000 - 1000.
	assert ld.rtt.has_sample
	assert ld.rtt.latest_rtt == 1000 * time.nanosecond
}

// test_packet_threshold_only_loss_detection is the plan's own explicitly
// named test: loss declared purely via RFC 9002 §6.1.1's kPacketThreshold
// rule, with `now` chosen so the time-threshold (which floors at
// kGranularity = 1ms) cannot possibly also be met.
fn test_packet_threshold_only_loss_detection() {
	mut ld := new_quic_loss_detection_timer()
	base := u64(1_000_000_000)
	for pn in u64(0) .. 5 {
		ld.on_packet_sent(.application_data, pn, 1200, true, true, base)
	}

	ack := AckFrame{
		largest_acknowledged: 4
		ack_delay:            0
		ranges:               [AckRange{
			smallest: 4
			largest:  4
		}]
	}
	// Only 1us after send -- far below any possible loss_delay (floored at
	// 1ms), so the time threshold cannot be what triggers this.
	now := base + 1000
	result := ld.on_ack_received(.application_data, ack, default_ack_delay_exponent,
		25 * time.millisecond, false, now)

	assert result.newly_acked.len == 1
	assert result.newly_acked[0].packet_number == 4

	// pn0, pn1: largest_acked(4) >= pn+3 -> 4>=3, 4>=4 both true -> lost.
	// pn2, pn3: 4>=5, 4>=6 both false -> still outstanding, not lost.
	lost_pns := result.lost.map(it.packet_number)
	assert lost_pns.len == 2
	assert 0 in lost_pns
	assert 1 in lost_pns
	assert ld.application_data.sent_packets.len == 2
	assert 2 in ld.application_data.sent_packets
	assert 3 in ld.application_data.sent_packets
}

// test_time_threshold_only_loss_detection is the plan's own explicitly
// named test: loss declared purely via RFC 9002 §6.1.2's time threshold,
// with only 2 packets outstanding so the packet-threshold rule (needs
// largest_acked >= pn+3) can never fire.
fn test_time_threshold_only_loss_detection() {
	mut ld := new_quic_loss_detection_timer()
	now := u64(1_000_000_000_000)
	t1 := now - 100_000_000 // pn1 sent 100ms before ack processing
	t0 := now - 200_000_000 // pn0 sent 200ms before ack processing

	ld.on_packet_sent(.application_data, 0, 1200, true, true, t0)
	ld.on_packet_sent(.application_data, 1, 1200, true, true, t1)

	ack := AckFrame{
		largest_acknowledged: 1
		ack_delay:            0
		ranges:               [AckRange{
			smallest: 1
			largest:  1
		}]
	}
	result := ld.on_ack_received(.application_data, ack, default_ack_delay_exponent,
		25 * time.millisecond, false, now)

	// RTT sample = now - t1 = 100ms (first sample) -> loss_delay = 9/8*100ms
	// = 112.5ms; pn0's age (200ms) exceeds that, so it's declared lost.
	assert ld.rtt.smoothed_rtt == 100 * time.millisecond
	assert result.newly_acked.len == 1
	assert result.lost.len == 1
	assert result.lost[0].packet_number == 0
	assert ld.application_data.sent_packets.len == 0
	assert ld.application_data.loss_time == none
}

fn test_loss_time_and_space_picks_earliest_across_spaces() {
	mut ld := new_quic_loss_detection_timer()
	ld.initial.loss_time = 5000
	ld.handshake.loss_time = 3000
	ld.application_data.loss_time = 9000

	loss_time, loss_space := ld.loss_time_and_space() or {
		assert false, 'expected a loss_time to be found'
		return
	}
	assert loss_time == 3000
	assert loss_space == .handshake
}

fn test_loss_time_and_space_none_when_no_space_has_a_pending_loss_time() {
	mut ld := new_quic_loss_detection_timer()
	if _, _ := ld.loss_time_and_space() {
		assert false, 'expected no loss_time to be pending'
	}
}

// test_pto_time_and_space_single_timer_sourced_from_earliest_expiring_space
// is the plan's own explicitly named test: there is exactly ONE PTO timer
// for the whole connection, sourced from whichever space's own deadline is
// earliest -- not three independent per-space PTO timers (a distinct rule
// from packet numbers, which genuinely ARE independent per space).
fn test_pto_time_and_space_single_timer_sourced_from_earliest_expiring_space() {
	mut ld := new_quic_loss_detection_timer()
	ld.on_packet_sent(.initial, 0, 1200, true, true, 1000)
	ld.on_packet_sent(.handshake, 0, 1200, true, true, 5000)

	// Fresh estimator: pto_period() = 999ms; pto_count=0 -> backoff=1.
	timeout, space := ld.pto_time_and_space(false, time.Duration(0))
	assert space == .initial
	assert timeout == 1000 + 999_000_000
}

fn test_pto_time_and_space_excludes_application_data_before_handshake_confirmed() {
	mut ld := new_quic_loss_detection_timer()
	ld.on_packet_sent(.initial, 0, 1200, true, true, 1000)
	// application_data has the earliest RAW deadline, but must be excluded
	// entirely while the handshake is not yet confirmed.
	ld.on_packet_sent(.application_data, 0, 1200, true, true, 500)

	timeout, space := ld.pto_time_and_space(false, time.Duration(0))
	assert space == .initial
	assert timeout == 1000 + 999_000_000
}

fn test_pto_time_and_space_includes_application_data_once_handshake_confirmed() {
	mut ld := new_quic_loss_detection_timer()
	ld.on_packet_sent(.initial, 0, 1200, true, true, 1000)
	ld.on_packet_sent(.application_data, 0, 1200, true, true, 500)

	timeout, space := ld.pto_time_and_space(true, time.Duration(0))
	assert space == .application_data
	assert timeout == 500 + 999_000_000
}

fn test_pto_time_and_space_ignores_spaces_with_no_in_flight_ack_eliciting_packet() {
	mut ld := new_quic_loss_detection_timer()
	// Not ack-eliciting/not in-flight: does not arm the PTO for its space.
	ld.on_packet_sent(.initial, 0, 1200, false, false, 1000)
	ld.on_packet_sent(.handshake, 0, 1200, true, true, 5000)

	timeout, space := ld.pto_time_and_space(false, time.Duration(0))
	assert space == .handshake
	assert timeout == 5000 + 999_000_000
}

fn test_on_ack_received_resets_pto_count_only_when_nothing_lost() {
	mut ld := new_quic_loss_detection_timer()
	ld.pto_count = 3

	ld.on_packet_sent(.application_data, 0, 1200, true, true, 1000)
	ack := AckFrame{
		largest_acknowledged: 0
		ack_delay:            0
		ranges:               [AckRange{
			smallest: 0
			largest:  0
		}]
	}
	ld.on_ack_received(.application_data, ack, default_ack_delay_exponent, 25 * time.millisecond,
		false, 2000)
	assert ld.pto_count == 0
}

fn test_on_ack_received_leaves_pto_count_untouched_when_something_lost() {
	mut ld := new_quic_loss_detection_timer()
	ld.pto_count = 3

	base := u64(1_000_000_000)
	for pn in u64(0) .. 5 {
		ld.on_packet_sent(.application_data, pn, 1200, true, true, base)
	}
	ack := AckFrame{
		largest_acknowledged: 4
		ack_delay:            0
		ranges:               [AckRange{
			smallest: 4
			largest:  4
		}]
	}
	result := ld.on_ack_received(.application_data, ack, default_ack_delay_exponent,
		25 * time.millisecond, false, base + 1000)
	assert result.lost.len > 0
	assert ld.pto_count == 3
}

fn test_on_loss_detection_timeout_fires_pto_when_no_loss_time_pending() {
	mut ld := new_quic_loss_detection_timer()
	ld.on_packet_sent(.initial, 0, 1200, true, true, 1000)
	assert ld.pto_count == 0

	result := ld.on_loss_detection_timeout(999_001_000, false, time.Duration(0))
	assert result.pto_fired
	assert result.pto_space == .initial
	assert result.lost.len == 0
	assert ld.pto_count == 1
}

fn test_on_loss_detection_timeout_reports_loss_when_a_loss_time_is_pending() {
	mut ld := new_quic_loss_detection_timer()
	ld.initial.loss_time = 5000
	ld.initial.largest_acked_packet = 10
	ld.initial.sent_packets[3] = SentPacketInfo{
		packet_number:    3
		time_sent:        100
		sent_bytes:       1200
		is_ack_eliciting: true
		in_flight:        true
	}

	result := ld.on_loss_detection_timeout(1_000_000, false, time.Duration(0))
	assert !result.pto_fired
	assert result.lost.len == 1
	assert result.lost[0].packet_number == 3
	// Only an actual PTO firing increments pto_count -- a loss-timeout
	// firing must leave it untouched.
	assert ld.pto_count == 0
}

// The next 2 tests are Phase-R regressions for gaps found via a full RFC
// 9002 §7.6/Appendix A.9 audit while reviewing this PR (2026-08-11), not
// from an external review round: is_persistent_congestion's own congestion-
// period formula was missing RFC 9002 §7.6.1's max_ack_delay term and
// §7.6.2's "a prior RTT sample existed when these packets were sent"
// condition entirely.
//
// That same audit's own FIRST-DRAFT fix for the max_ack_delay gap was
// itself wrong -- it zeroed max_ack_delay for Initial/Handshake spaces,
// copying pto_time_and_space's per-space caveat, which RFC 9002 §7.6.1
// explicitly says does NOT apply here ("Unlike the PTO computation...this
// duration includes the max_ack_delay irrespective of the packet number
// spaces"). It also wrongly added a persistent_congestion verdict to
// on_loss_detection_timeout's timer-triggered path, contradicting §7.6.2's
// opening sentence ("A sender establishes persistent congestion after the
// receipt of an acknowledgment"). Both caught by a maintainer-run "Local AI
// Review" on this PR and fixed here; see
// test_on_ack_received_persistent_congestion_includes_max_ack_delay_for_initial_space
// below for the caller-side regression covering the first mistake -- the
// second mistake needed no replacement test, since removing
// LossTimeoutResult.persistent_congestion entirely makes the wrong behavior
// impossible to reintroduce by accident.

fn test_is_persistent_congestion_requires_prior_rtt_sample_existed_at_send_time() {
	pto := 100 * time.millisecond
	lost := [
		SentPacketInfo{
			packet_number:    0
			time_sent:        1_000_000_000
			sent_bytes:       1200
			is_ack_eliciting: true
			in_flight:        true
		},
		SentPacketInfo{
			packet_number:    1
			time_sent:        1_000_000_000 + u64(4 * pto)
			sent_bytes:       1200
			is_ack_eliciting: true
			in_flight:        true
		},
	]
	// No RTT sample has ever been taken.
	assert !is_persistent_congestion(lost, pto, time.Duration(0), none)
	// A sample exists, but only AFTER the earliest lost packet was sent --
	// it did not exist yet AT that packet's own send time.
	assert !is_persistent_congestion(lost, pto, time.Duration(0), u64(1_000_000_001))
	// A sample existed at-or-before the earliest lost packet's send time.
	assert is_persistent_congestion(lost, pto, time.Duration(0), u64(1_000_000_000))
}

fn test_is_persistent_congestion_congestion_period_includes_max_ack_delay() {
	pto := 100 * time.millisecond
	max_ack_delay := 50 * time.millisecond
	// Span is 350ms: enough to qualify against a bare 3*pto=300ms
	// threshold, but NOT enough once RFC 9002 §7.6.1 correctly folds
	// max_ack_delay into the congestion-period formula: (100+50)*3=450ms.
	lost := [
		SentPacketInfo{
			packet_number:    0
			time_sent:        1_000_000_000
			sent_bytes:       1200
			is_ack_eliciting: true
			in_flight:        true
		},
		SentPacketInfo{
			packet_number:    1
			time_sent:        1_000_000_000 + u64(350 * time.millisecond)
			sent_bytes:       1200
			is_ack_eliciting: true
			in_flight:        true
		},
	]
	assert !is_persistent_congestion(lost, pto, max_ack_delay, u64(1_000_000_000))
	assert is_persistent_congestion(lost, pto, time.Duration(0), u64(1_000_000_000))
}

// test_on_ack_received_persistent_congestion_includes_max_ack_delay_for_initial_space
// is the caller-side regression for the "Local AI Review" finding above:
// on_ack_received must pass max_ack_delay through to is_persistent_congestion
// UNCHANGED for the Initial space too, not zeroed the way pto_time_and_space
// correctly zeroes it for the (different) PTO formula.
fn test_on_ack_received_persistent_congestion_includes_max_ack_delay_for_initial_space() {
	mut ld := new_quic_loss_detection_timer()
	max_ack_delay := 500 * time.millisecond
	ld.first_rtt_sample_time = 50
	ld.initial.largest_acked_packet = 10
	ld.initial.sent_packets[3] = SentPacketInfo{
		packet_number:    3
		time_sent:        100
		sent_bytes:       1200
		is_ack_eliciting: true
		in_flight:        true
	}
	ld.initial.sent_packets[4] = SentPacketInfo{
		packet_number: 4
		// Span of 3.5s: >= 3*pto_period() (~3s, the WRONG max_ack_delay-
		// zeroed threshold a caller-side regression would use), but <
		// 3*(pto_period()+max_ack_delay) (~4.5s, the correct RFC 9002
		// §7.6.1 threshold) -- only distinguishes the two if
		// on_ack_received actually forwards max_ack_delay unchanged.
		time_sent:        100 + u64(3500 * time.millisecond)
		sent_bytes:       1200
		is_ack_eliciting: true
		in_flight:        true
	}
	// A non-ack-eliciting packet we ack here purely to drive
	// on_ack_received's persistent-congestion computation without
	// disturbing pn3/pn4 above or taking an RTT sample (which would make
	// pto_period() unpredictable for this test).
	ld.initial.sent_packets[10] = SentPacketInfo{
		packet_number:    10
		time_sent:        50
		sent_bytes:       1200
		is_ack_eliciting: false
		in_flight:        false
	}
	ack := AckFrame{
		largest_acknowledged: 10
		ack_delay:            0
		ranges:               [AckRange{
			smallest: 10
			largest:  10
		}]
	}
	result := ld.on_ack_received(.initial, ack, default_ack_delay_exponent, max_ack_delay, false,
		4_000_000_000)
	// pn3, pn4 both satisfy the packet threshold (largest_acked=10 >=
	// pn+3), so both are lost regardless of `now`.
	assert result.lost.len == 2
	assert !result.persistent_congestion
}

fn test_is_persistent_congestion_contiguous_batch_spanning_three_ptos() {
	pto := 100 * time.millisecond
	lost := [
		SentPacketInfo{
			packet_number:    0
			time_sent:        1_000_000_000
			sent_bytes:       1200
			is_ack_eliciting: true
			in_flight:        true
		},
		SentPacketInfo{
			packet_number:    1
			time_sent:        1_000_000_000 + u64(4 * pto)
			sent_bytes:       1200
			is_ack_eliciting: true
			in_flight:        true
		},
	]
	assert is_persistent_congestion(lost, pto, time.Duration(0), u64(0))
}

fn test_is_persistent_congestion_requires_span_at_least_three_ptos() {
	pto := 100 * time.millisecond
	lost := [
		SentPacketInfo{
			packet_number:    0
			time_sent:        1_000_000_000
			sent_bytes:       1200
			is_ack_eliciting: true
			in_flight:        true
		},
		SentPacketInfo{
			packet_number:    1
			time_sent:        1_000_000_000 + u64(2 * pto) // only 2 PTOs apart
			sent_bytes:       1200
			is_ack_eliciting: true
			in_flight:        true
		},
	]
	assert !is_persistent_congestion(lost, pto, time.Duration(0), u64(0))
}

fn test_is_persistent_congestion_requires_contiguous_packet_numbers() {
	pto := 100 * time.millisecond
	// pn0 and pn2 both lost, spanning enough time, but pn1 -- which must
	// ALSO have been declared lost for persistent congestion to hold -- is
	// absent from this batch (it survived: acked, or still outstanding).
	lost := [
		SentPacketInfo{
			packet_number:    0
			time_sent:        1_000_000_000
			sent_bytes:       1200
			is_ack_eliciting: true
			in_flight:        true
		},
		SentPacketInfo{
			packet_number:    2
			time_sent:        1_000_000_000 + u64(4 * pto)
			sent_bytes:       1200
			is_ack_eliciting: true
			in_flight:        true
		},
	]
	assert !is_persistent_congestion(lost, pto, time.Duration(0), u64(0))
}

fn test_is_persistent_congestion_requires_at_least_two_ack_eliciting_packets() {
	pto := 100 * time.millisecond
	lost := [
		SentPacketInfo{
			packet_number:    0
			time_sent:        1_000_000_000
			sent_bytes:       1200
			is_ack_eliciting: true
			in_flight:        true
		},
	]
	assert !is_persistent_congestion(lost, pto, time.Duration(0), u64(0))
}
