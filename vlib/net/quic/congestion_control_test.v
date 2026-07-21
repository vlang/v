module quic

fn test_new_newreno_starts_in_slow_start_with_initial_window() {
	c := new_newreno_congestion_control()
	assert c.congestion_window == initial_window
	assert c.bytes_in_flight == 0
	assert c.ssthresh == none
	assert c.is_in_slow_start()
}

fn test_on_packet_sent_cc_accumulates_bytes_in_flight() {
	mut c := new_newreno_congestion_control()
	c.on_packet_sent_cc(1200)
	c.on_packet_sent_cc(1200)
	assert c.bytes_in_flight == 2400
}

fn test_on_packets_acked_grows_window_fully_in_slow_start() {
	mut c := new_newreno_congestion_control()
	c.on_packet_sent_cc(1200)

	acked := [
		SentPacketInfo{
			packet_number:    0
			time_sent:        1000
			sent_bytes:       1200
			is_ack_eliciting: true
			in_flight:        true
		},
	]
	c.on_packets_acked(acked)

	assert c.bytes_in_flight == 0
	assert c.congestion_window == initial_window + 1200
}

fn test_on_packets_acked_grows_window_proportionally_in_congestion_avoidance() {
	mut c := NewRenoCongestionControl{
		congestion_window: 12000
		ssthresh:          6000
	}
	acked := [
		SentPacketInfo{
			packet_number:    0
			time_sent:        1000
			sent_bytes:       1200
			is_ack_eliciting: true
			in_flight:        true
		},
	]
	c.on_packets_acked(acked)
	// max_datagram_size(1200) * sent_bytes(1200) / cwnd(12000) = 120
	assert c.congestion_window == 12120
}

// test_slow_start_resumes_after_persistent_congestion_drops_below_ssthresh
// confirms is_in_slow_start() re-derives `congestion_window < ssthresh`
// rather than shortcutting to "a loss has ever happened": after a
// persistent-congestion collapse leaves cwnd below an already-set,
// larger ssthresh left over from the congestion event that triggered it,
// growth must go back to full slow-start increments, not the AIMD
// formula, until cwnd climbs back up to that ssthresh.
fn test_slow_start_resumes_after_persistent_congestion_drops_below_ssthresh() {
	mut c := NewRenoCongestionControl{
		congestion_window: 2400 // == minimum_window, as if just collapsed
		ssthresh:          6000 // left over from the triggering congestion event
	}
	assert c.is_in_slow_start()

	acked := [
		SentPacketInfo{
			packet_number:    0
			time_sent:        1000
			sent_bytes:       1200
			is_ack_eliciting: true
			in_flight:        true
		},
	]
	c.on_packets_acked(acked)
	assert c.congestion_window == 2400 + 1200 // full slow-start growth
}

fn test_on_packets_acked_does_not_grow_window_during_recovery() {
	mut c := new_newreno_congestion_control()
	c.congestion_recovery_start_time = 5000
	before := c.congestion_window

	// Sent at-or-before recovery start: part of the episode that triggered
	// recovery, must not grow cwnd.
	acked := [
		SentPacketInfo{
			packet_number:    0
			time_sent:        3000
			sent_bytes:       1200
			is_ack_eliciting: true
			in_flight:        true
		},
	]
	c.on_packets_acked(acked)
	assert c.congestion_window == before

	// Sent AFTER recovery started: a genuinely new packet, does grow cwnd.
	acked2 := [
		SentPacketInfo{
			packet_number:    1
			time_sent:        6000
			sent_bytes:       1200
			is_ack_eliciting: true
			in_flight:        true
		},
	]
	c.on_packets_acked(acked2)
	assert c.congestion_window == before + 1200
}

fn test_on_packets_lost_ordinary_loss_halves_window_via_ssthresh() {
	mut c := new_newreno_congestion_control()
	c.on_packet_sent_cc(1200)

	lost := [
		SentPacketInfo{
			packet_number:    0
			time_sent:        1000
			sent_bytes:       1200
			is_ack_eliciting: true
			in_flight:        true
		},
	]
	c.on_packets_lost(lost, false, 5000)

	assert c.bytes_in_flight == 0
	assert c.ssthresh or { 0 } == 6000 // initial_window(12000) * 0.5
	assert c.congestion_window == 6000
	assert c.congestion_recovery_start_time or { 0 } == 5000
}

// test_on_packets_lost_persistent_congestion_collapses_to_minimum_window
// is the plan's own explicitly named "persistent-congestion collapse"
// test, distinguished from ordinary-loss ssthresh halving above by its
// harsher direct reset to kMinimumWindow.
fn test_on_packets_lost_persistent_congestion_collapses_to_minimum_window() {
	mut c := new_newreno_congestion_control()
	c.on_packet_sent_cc(1200)

	lost := [
		SentPacketInfo{
			packet_number:    0
			time_sent:        1000
			sent_bytes:       1200
			is_ack_eliciting: true
			in_flight:        true
		},
	]
	c.on_packets_lost(lost, true, 5000)

	assert c.congestion_window == minimum_window
	assert c.congestion_recovery_start_time == none
}

// test_single_reaction_per_recovery_episode is the plan's own explicitly
// named test: multiple losses within the SAME recovery episode must
// collapse the window exactly once, not once per lost packet; a loss from
// a genuinely LATER episode (sent after the current recovery began) must
// still react again.
fn test_single_reaction_per_recovery_episode() {
	mut c := new_newreno_congestion_control() // cwnd = 12000
	c.on_packet_sent_cc(3600)

	first_loss := [
		SentPacketInfo{
			packet_number:    0
			time_sent:        1000
			sent_bytes:       1200
			is_ack_eliciting: true
			in_flight:        true
		},
	]
	c.on_packets_lost(first_loss, false, 5000)
	assert c.congestion_window == 6000 // 12000 * 0.5
	assert c.congestion_recovery_start_time or { 0 } == 5000

	// Sent at time 2000, BEFORE the recovery episode started (5000) --
	// belongs to the same episode, must NOT trigger a second halving.
	second_loss := [
		SentPacketInfo{
			packet_number:    1
			time_sent:        2000
			sent_bytes:       1200
			is_ack_eliciting: true
			in_flight:        true
		},
	]
	c.on_packets_lost(second_loss, false, 6000)
	assert c.congestion_window == 6000 // unchanged: still one reaction
	assert c.congestion_recovery_start_time or { 0 } == 5000 // unchanged

	// Sent at time 7000, AFTER the recovery episode started (5000) -- a
	// genuinely new loss episode, must react again.
	third_loss := [
		SentPacketInfo{
			packet_number:    2
			time_sent:        7000
			sent_bytes:       1200
			is_ack_eliciting: true
			in_flight:        true
		},
	]
	c.on_packets_lost(third_loss, false, 8000)
	assert c.congestion_window == 3000 // 6000 * 0.5
	assert c.congestion_recovery_start_time or { 0 } == 8000
}

fn test_on_packets_lost_with_empty_batch_is_a_no_op() {
	mut c := new_newreno_congestion_control()
	before := c.congestion_window
	c.on_packets_lost([]SentPacketInfo{}, false, 5000)
	assert c.congestion_window == before
	assert c.congestion_recovery_start_time == none
}
