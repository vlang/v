module quic

import hash

fn sent_frames_test_info(pns ...u64) []SentPacketInfo {
	return pns.map(SentPacketInfo{
		packet_number: it
	})
}

fn sent_frames_test_texts(frames []&SentFrame) []string {
	return frames.map(it.bytes.bytestr())
}

fn sent_frames_test_frame(text string) &SentFrame {
	return &SentFrame{
		bytes: text.bytes()
	}
}

// Every lost packet of a list gives back every frame it carried, once.
fn test_lost_packets_queue_every_frame_once() {
	mut s := SentFrames{}
	s.record(4, [sent_frames_test_frame('a'), sent_frames_test_frame('b')])
	s.record(5, [sent_frames_test_frame('c')])
	s.record(6, [sent_frames_test_frame('d')])
	s.on_lost(sent_frames_test_info(4, 5))
	s.on_lost(sent_frames_test_info(4, 5))
	assert sent_frames_test_texts(s.take_lost()) == ['a', 'b', 'c']
	assert s.take_lost().len == 0
	assert sent_frames_test_texts(s.oldest_unacked()) == ['d']
}

// An acknowledged packet's frames are never sent again, for every packet of
// the list.
fn test_acknowledged_packets_are_forgotten() {
	mut s := SentFrames{}
	s.record(1, [sent_frames_test_frame('a')])
	s.record(2, [sent_frames_test_frame('b')])
	s.on_acked(sent_frames_test_info(1, 2))
	assert s.in_flight.len == 0
	s.on_lost(sent_frames_test_info(1, 2))
	assert s.take_lost().len == 0
}

// While a copy is in flight the loss of another copy queues nothing; once
// all are lost the frame is queued once.
fn test_a_frame_is_queued_when_its_last_copy_is_lost() {
	mut s := SentFrames{}
	frame := sent_frames_test_frame('copied')
	s.record(1, [frame])
	s.record(2, [frame])
	s.on_lost(sent_frames_test_info(1))
	assert s.take_lost().len == 0, 'queued while a copy is in flight'
	s.on_lost(sent_frames_test_info(2))
	assert sent_frames_test_texts(s.take_lost()) == ['copied']
}

// Once any copy is acknowledged, losing the others queues nothing, and the
// probe skips them.
fn test_acknowledging_one_copy_ends_every_copy() {
	mut s := SentFrames{}
	frame := sent_frames_test_frame('copied')
	s.record(1, [frame])
	s.record(2, [frame])
	s.record(3, [sent_frames_test_frame('later')])
	s.on_acked(sent_frames_test_info(2))
	assert sent_frames_test_texts(s.oldest_unacked()) == ['later']
	s.on_lost(sent_frames_test_info(1))
	assert s.take_lost().len == 0
}

// The probe repeats the lowest packet number's unacknowledged frames only.
fn test_oldest_unacked_is_the_lowest_packet_number() {
	mut s := SentFrames{}
	assert s.oldest_unacked().len == 0
	acked := sent_frames_test_frame('acked')
	s.record(9, [sent_frames_test_frame('nine')])
	s.record(3, [acked, sent_frames_test_frame('three')])
	s.record(7, [acked])
	s.record(12, [sent_frames_test_frame('twelve')])
	s.on_acked(sent_frames_test_info(7))
	assert sent_frames_test_texts(s.oldest_unacked()) == ['three']
}

// A lower packet whose frames were all acknowledged in another one does not
// hide the packets above it.
fn test_oldest_unacked_skips_an_acknowledged_lower_packet() {
	mut s := SentFrames{}
	acked := sent_frames_test_frame('acked')
	s.record(9, [sent_frames_test_frame('nine')])
	s.record(3, [acked])
	s.record(4, [acked])
	s.on_acked(sent_frames_test_info(4))
	assert sent_frames_test_texts(s.oldest_unacked()) == ['nine']
}

// Equal frames sent separately (a peer that asks for one retirement many
// times) are queued once, so a peer that acknowledges none of them cannot
// make the queue grow.
fn test_equal_lost_frames_are_queued_once() {
	mut s := SentFrames{}
	s.record(1, [sent_frames_test_frame('same'), sent_frames_test_frame('other')])
	s.record(2, [sent_frames_test_frame('same')])
	s.record(3, [sent_frames_test_frame('same')])
	s.on_lost(sent_frames_test_info(1, 2))
	s.on_lost(sent_frames_test_info(3))
	assert sent_frames_test_texts(s.take_lost()) == ['same', 'other']
}

// Every frame of an acknowledged packet is ended in its other copies.
fn test_acknowledging_a_packet_ends_each_of_its_frames() {
	mut s := SentFrames{}
	frames := [sent_frames_test_frame('a'), sent_frames_test_frame('b'), sent_frames_test_frame('c')]
	s.record(1, frames)
	s.record(2, frames)
	s.on_acked(sent_frames_test_info(2))
	assert s.oldest_unacked().len == 0
	s.on_lost(sent_frames_test_info(1))
	assert s.take_lost().len == 0
	assert frames.all(it.copies == 0)
}

// A packet with nothing to repair leaves no record.
fn test_recording_no_frames_keeps_nothing() {
	mut s := SentFrames{}
	s.record(1, [])
	assert s.in_flight.len == 0
}

// A peer chooses the sequence numbers it has retired, so it can choose frames
// whose hashes collide. Enough of them overflow a map that is keyed by them.
// (These collide in a build without -prod, where maps use this hash.)
fn test_lost_frames_with_colliding_hashes_are_queued() {
	mut s := SentFrames{}
	mut want := []string{}
	for seq := u64(1); want.len < 300; seq++ {
		frame := encode_retire_connection_id_frame(seq)!
		if hash.sum64(frame, 0) & 0xfff == 0 {
			want << frame.bytestr()
			s.record(u64(want.len), [&SentFrame{ bytes: frame }])
			s.on_lost(sent_frames_test_info(u64(want.len)))
		}
	}
	assert sent_frames_test_texts(s.take_lost()) == want
}

// Frames come back in the order they were first lost, whatever their bytes.
fn test_lost_frames_keep_the_order_they_were_lost_in() {
	mut s := SentFrames{}
	mut want := []string{}
	for i, text in ['m', 'z', 'a', 'z', 'k', 'a', 'm', 'b'] {
		s.record(u64(i), [sent_frames_test_frame(text)])
		s.on_lost(sent_frames_test_info(u64(i)))
		if text !in want {
			want << text
		}
	}
	assert sent_frames_test_texts(s.take_lost()) == want
}

// Equal frames compare by position, since the C library's sort may reorder
// elements that compare equal.
fn test_equal_lost_frames_compare_by_position() {
	frame := sent_frames_test_frame('same')
	first := LostFrame{
		frame:    frame
		bytes:    'same'
		position: 1
	}
	second := LostFrame{
		frame:    frame
		bytes:    'same'
		position: 2
	}
	assert compare_lost_frames(&first, &second) < 0
	assert compare_lost_frames(&second, &first) > 0
}
