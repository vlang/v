module quic

// SentFrameKind tells the frames that can become obsolete (see
// QuicConn.is_obsolete) from the others.
enum SentFrameKind {
	other
	stream
	max_data
	max_stream_data
	streams_blocked_bidi
	streams_blocked_uni
}

// SentFrame is one encoded frame whose information is sent again when it is
// lost (RFC 9000 §13.3). Every packet carrying a copy shares the same
// SentFrame, so acknowledging any one of them ends them all.
@[heap]
struct SentFrame {
	bytes []u8
	kind  SentFrameKind
	id    u64 // the stream of a .stream or .max_stream_data frame
	limit u64 // the limit a .max_* or .streams_blocked_* frame carries
mut:
	acked  bool
	copies int // packets in flight carrying this frame
}

// SentFrames tracks, for one packet number space, the frames to send again
// if the packets carrying them are lost. `in_flight` maps a sent packet
// number to its frames, and holds only packets still awaiting an
// acknowledgment; `lost` holds the frames waiting to be sent again, in the
// order they were lost.
@[heap]
struct SentFrames {
mut:
	in_flight map[u64][]&SentFrame
	lost      []&SentFrame
}

// LostFrame is a queued frame and its place in the queue, for sorting.
struct LostFrame {
	frame    &SentFrame
	bytes    string
	position int
}

// record notes that packet `packet_number` carried `frames`.
fn (mut s SentFrames) record(packet_number u64, frames []&SentFrame) {
	if frames.len == 0 {
		return
	}
	mut carried := frames.clone()
	for mut frame in carried {
		frame.copies++
	}
	s.in_flight[packet_number] = carried
}

// on_acked forgets acknowledged packets: the peer has their frames.
fn (mut s SentFrames) on_acked(acked []SentPacketInfo) {
	for info in acked {
		mut frames := s.in_flight[info.packet_number] or { continue }
		s.in_flight.delete(info.packet_number)
		for mut frame in frames {
			frame.acked = true
			frame.copies--
		}
	}
}

// on_lost queues the frames of lost packets to be sent again, except those
// acknowledged in, or still in flight in, another packet.
fn (mut s SentFrames) on_lost(lost []SentPacketInfo) {
	for info in lost {
		mut frames := s.in_flight[info.packet_number] or { continue }
		s.in_flight.delete(info.packet_number)
		for mut frame in frames {
			frame.copies--
			if !frame.acked && frame.copies == 0 {
				s.lost << frame
			}
		}
	}
}

// take_lost empties the queue of frames to send again and returns it without
// the frames equal to an earlier one. It sorts to find those: the peer chooses
// the frames, so comparing each with the rest, or hashing them, would let it
// choose the cost, and enough colliding keys make a map panic.
fn (mut s SentFrames) take_lost() []&SentFrame {
	mut queued := []LostFrame{cap: s.lost.len}
	for i, frame in s.lost {
		queued << LostFrame{
			frame:    frame
			bytes:    frame.bytes.bytestr()
			position: i
		}
	}
	s.lost = []
	queued.sort_with_compare(compare_lost_frames)
	mut kept := []LostFrame{cap: queued.len}
	for i, entry in queued {
		if i == 0 || entry.bytes != queued[i - 1].bytes {
			kept << entry
		}
	}
	kept.sort(a.position < b.position)
	return kept.map(it.frame)
}

// compare_lost_frames orders frames by their bytes, equal ones by position.
fn compare_lost_frames(a &LostFrame, b &LostFrame) int {
	by_bytes := compare_strings(a.bytes, b.bytes)
	return if by_bytes == 0 { a.position - b.position } else { by_bytes }
}

// oldest_unacked returns the unacknowledged frames of the lowest-numbered
// packet that has any: what a PTO probe sends again (RFC 9002 §6.2.4).
fn (s &SentFrames) oldest_unacked() []&SentFrame {
	mut oldest := []&SentFrame{}
	mut oldest_pn := u64(0)
	for pn, frames in s.in_flight {
		if oldest.len > 0 && pn > oldest_pn {
			continue
		}
		unacked := frames.filter(!it.acked)
		if unacked.len > 0 {
			oldest = unacked.clone()
			oldest_pn = pn
		}
	}
	return oldest
}
