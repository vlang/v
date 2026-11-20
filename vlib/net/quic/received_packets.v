module quic

// max_ack_ranges caps how many ACK ranges a ReceivedPacketSet keeps. RFC 9000
// §13.2.3: "A receiver limits the number of ACK Ranges (Section 19.3.1) it
// remembers and sends in ACK frames, both to limit the size of ACK frames and
// to avoid resource exhaustion." 32 ranges encode in well under 600 bytes.
const max_ack_ranges = 32

// ReceivedPacketSet is what this endpoint has received in one packet number
// space: the ranges it still acknowledges, plus a floor at or below which
// every packet number counts as already received. Together they give the
// next ACK frame's ranges (RFC 9000 §13.2.3) and duplicate detection (§12.3)
// in bounded state.
struct ReceivedPacketSet {
mut:
	// ranges is largest first; ranges never overlap or touch.
	ranges []AckRange
	// floor, once set, makes every packet number <= floor a duplicate. It
	// rises whenever ranges are dropped, because RFC 9000 §13.2.3 says "A
	// receiver MUST retain an ACK Range unless it can ensure that it will
	// not subsequently accept packets with numbers in that range."
	floor ?u64
}

fn (s &ReceivedPacketSet) is_empty() bool {
	return s.ranges.len == 0
}

// contains reports whether `pn` was already received, or must be treated as
// received: it is at or below the floor, or the set is full and `pn` lies
// below every range without extending the smallest one. Inserting such a
// packet would evict it at once, so processing it would leave an
// ack-eliciting packet that no ACK frame ever covers (RFC 9000 §13.2.1).
fn (s &ReceivedPacketSet) contains(pn u64) bool {
	if floor := s.floor {
		if pn <= floor {
			return true
		}
	}
	if s.ranges.len >= max_ack_ranges && pn + 1 < s.ranges.last().smallest {
		return true
	}
	for r in s.ranges {
		if pn > r.largest {
			return false
		}
		if pn >= r.smallest {
			return true
		}
	}
	return false
}

// insert records `pn`; a packet number already contained is ignored. When
// that makes more than max_ack_ranges ranges, the smallest range is dropped
// and the floor rises to cover it (RFC 9000 §13.2.3: "older ranges (those
// with the smallest packet numbers) are omitted").
fn (mut s ReceivedPacketSet) insert(pn u64) {
	if s.contains(pn) {
		return
	}
	mut i := 0
	for i < s.ranges.len && s.ranges[i].smallest > pn {
		i++
	}
	// Every range before i lies above pn; range i, if any, lies below it.
	joins_above := i > 0 && s.ranges[i - 1].smallest == pn + 1
	joins_below := i < s.ranges.len && s.ranges[i].largest + 1 == pn
	if joins_above && joins_below {
		s.ranges[i - 1] = AckRange{
			smallest: s.ranges[i].smallest
			largest:  s.ranges[i - 1].largest
		}
		s.ranges.delete(i)
	} else if joins_above {
		s.ranges[i - 1] = AckRange{
			smallest: pn
			largest:  s.ranges[i - 1].largest
		}
	} else if joins_below {
		s.ranges[i] = AckRange{
			smallest: s.ranges[i].smallest
			largest:  pn
		}
	} else {
		s.ranges.insert(i, AckRange{
			smallest: pn
			largest:  pn
		})
	}
	if s.ranges.len > max_ack_ranges {
		dropped := s.ranges.pop()
		s.raise_floor(dropped.largest)
	}
}

// forget_through stops acknowledging every packet number <= `pn`, because a
// packet carrying an ACK frame with that Largest Acknowledged was itself
// acknowledged (RFC 9000 §13.2.4). Those packet numbers become duplicates.
fn (mut s ReceivedPacketSet) forget_through(pn u64) {
	s.raise_floor(pn)
	mut kept := []AckRange{cap: s.ranges.len}
	for r in s.ranges {
		if r.largest <= pn {
			break
		}
		kept << AckRange{
			smallest: if r.smallest > pn { r.smallest } else { pn + 1 }
			largest:  r.largest
		}
	}
	s.ranges = kept
}

fn (mut s ReceivedPacketSet) raise_floor(pn u64) {
	if floor := s.floor {
		if floor >= pn {
			return
		}
	}
	s.floor = pn
}
