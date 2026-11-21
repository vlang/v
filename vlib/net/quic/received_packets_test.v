module quic

fn rp_range(smallest u64, largest u64) AckRange {
	return AckRange{
		smallest: smallest
		largest:  largest
	}
}

fn test_insert_merges_adjacent_packet_numbers_largest_first() {
	mut s := ReceivedPacketSet{}
	assert s.is_empty()
	for pn in [u64(5), 1, 3, 2, 7] {
		assert !s.contains(pn)
		s.insert(pn)
	}
	assert s.ranges == [rp_range(7, 7), rp_range(5, 5), rp_range(1, 3)]
	s.insert(4) // joins [5] and [1, 3]
	assert s.ranges == [rp_range(7, 7), rp_range(1, 5)]
	s.insert(6) // joins [7] and [1, 5]
	assert s.ranges == [rp_range(1, 7)]
	s.insert(0)
	assert s.ranges == [rp_range(0, 7)]
	for pn in u64(0) .. 8 {
		assert s.contains(pn)
	}
	assert !s.contains(8)
}

fn test_contains_reports_gaps_as_not_received() {
	mut s := ReceivedPacketSet{}
	s.insert(10)
	s.insert(4)
	assert !s.contains(11)
	assert !s.contains(7)
	assert !s.contains(3)
	assert s.contains(4) && s.contains(10)
}

// RFC 9000 §13.2.3: the number of ranges is capped; dropping the smallest
// raises the floor, so its packets are still treated as received.
fn test_insert_caps_ranges_and_raises_the_floor() {
	mut s := ReceivedPacketSet{}
	for i in u64(0) .. max_ack_ranges + 3 {
		s.insert(i * 2)
	}
	assert s.ranges.len == max_ack_ranges
	assert s.ranges[0] == rp_range((max_ack_ranges + 2) * 2, (max_ack_ranges + 2) * 2)
	assert s.ranges.last() == rp_range(6, 6)
	assert s.floor? == 4
	assert s.contains(3), 'a gap below the floor counts as received'
	assert s.contains(0)
	assert !s.contains(5)
}

// RFC 9000 §13.2.4: once an ACK with Largest Acknowledged `pn` is itself
// acknowledged, packets up to `pn` are no longer acknowledged, and a range
// straddling `pn` keeps only its part above it.
fn test_forget_through_trims_ranges_and_raises_the_floor() {
	mut s := ReceivedPacketSet{}
	for pn in [u64(1), 2, 3, 4, 5, 8, 9] {
		s.insert(pn)
	}
	s.forget_through(3)
	assert s.ranges == [rp_range(8, 9), rp_range(4, 5)]
	assert s.contains(2), 'at or below the floor counts as received'
	s.forget_through(2) // never lowers the floor
	assert s.floor? == 3
	s.forget_through(9)
	assert s.is_empty()
	assert s.contains(7)
	assert !s.contains(10)
}

fn test_insert_ignores_a_packet_number_already_held() {
	mut s := ReceivedPacketSet{}
	for pn in [u64(1), 2, 3, 7] {
		s.insert(pn)
	}
	s.insert(2)
	s.insert(7)
	assert s.ranges == [rp_range(7, 7), rp_range(1, 3)]
}

// RFC 9000 §13.2.1: an ack-eliciting packet must be acknowledged at least
// once. At the cap, a packet below every range that does not touch the
// smallest one would be evicted by its own insert, so it counts as received
// and is dropped before processing. One that extends the smallest range is
// still accepted.
fn test_full_set_treats_a_packet_it_would_evict_as_received() {
	mut s := ReceivedPacketSet{}
	for i in u64(1) .. max_ack_ranges + 1 {
		s.insert(i * 10)
	}
	assert s.ranges.len == max_ack_ranges
	assert s.ranges.last() == rp_range(10, 10)
	assert s.floor == none
	assert s.contains(5), 'inserting 5 would evict it at once, so it must never be processed'
	assert !s.contains(9), '9 extends the smallest range and stays acknowledged'
	s.insert(9)
	assert s.ranges.last() == rp_range(9, 10)
	assert s.ranges.len == max_ack_ranges
}
