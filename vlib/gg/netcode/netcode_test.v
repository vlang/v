module netcode

import gg
import math
import time

fn test_packet_roundtrip() {
	mut p := new_packet()
	p.write_u8(7)
	p.write_u16(513)
	p.write_u32(123456)
	p.write_f32(1.5)
	p.write_string('gg')!
	p.write_vec3(gg.Vec3{1, 2, 3})
	mut r := from_bytes(p.bytes())
	assert r.read_u8()! == 7
	assert r.read_u16()! == 513
	assert r.read_u32()! == 123456
	assert math.abs(r.read_f32()! - 1.5) < 0.001
	assert r.read_string()! == 'gg'
	v := r.read_vec3()!
	assert v.x == 1 && v.y == 2 && v.z == 3
	assert r.remaining() == 0
}

fn test_datagram_header_symmetry() {
	mut wire := new_packet()
	wire.write_u16(1234)
	wire.write_u16(99)
	wire.write_u32(0xDEADBEEF)
	wire.write_u8(7)
	wire.write_u8(8)
	seq, ack, bits, body := decode_header(wire.bytes())!
	assert seq == 1234
	assert ack == 99
	assert bits == 0xDEADBEEF
	assert body == [u8(7), 8]
}

fn test_packet_truncation() {
	mut p := from_bytes([u8(1)])
	assert p.read_u8()! == 1
	if _ := p.read_u32() {
		assert false, 'expected truncation'
	}
}

fn test_snapshot_interpolation() {
	mut a := new_snapshot(0, 1)
	mut b := new_snapshot(2, 1)
	a.positions[0] = gg.Vec3{0, 0, 0}
	b.positions[0] = gg.Vec3{10, 0, 0}
	mid := interpolate(a, b, 0.5)
	assert math.abs(mid.positions[0].x - 5) < 0.001
	mut buf := new_snapshot_buffer(4)
	buf.push(a)
	buf.push(b)
	assert buf.items.len == 2
	assert buf.items[0].time == 0 && buf.items[1].time == 2
}

fn test_packet_rejects_oversized_string_and_bad_pos() {
	mut p := new_packet()
	long := 'x'.repeat(65536)
	if _ := p.write_string(long) {
		assert false, 'a string longer than 65535 bytes must be rejected'
	}
	mut q := from_bytes([u8(1), 2, 3, 4])
	q.pos = -5
	if _ := q.read_u32() {
		assert false, 'a negative pos must not bypass the bounds check'
	}
	q.pos = 2147483647
	if _ := q.read_u16() {
		assert false, 'an overflowing pos must not bypass the bounds check'
	}
}

fn test_listening_peer_replies_to_last_sender() {
	mut server := listen('127.0.0.1:0') or { panic(err) }
	defer { server.close() or {} }
	addr := server.conn.sock.address()!.str()
	mut client := connect(addr) or { panic(err) }
	defer { client.close() or {} }
	server.conn.set_read_timeout(2 * time.second)
	client.conn.set_read_timeout(2 * time.second)

	mut request := new_packet()
	request.write_u8(0xAB)
	client.send(request)!
	got := server.recv()!
	assert got.bytes() == [u8(0xAB)]
	assert server.has_sender

	mut reply := new_packet()
	reply.write_u8(0xCD)
	server.send(reply)!
	back := client.recv()!
	assert back.bytes() == [u8(0xCD)]
}

fn test_send_without_known_remote_fails() {
	mut peer := listen('127.0.0.1:0') or { panic(err) }
	defer { peer.close() or {} }
	mut p := new_packet()
	p.write_u8(1)
	if _ := peer.send(p) {
		assert false, 'send without a remote or last sender must fail'
	}
}

fn test_ack_and_sequence_wrap() {
	mut peer := listen('127.0.0.1:0') or { panic(err) }
	defer { peer.close() or {} }
	peer.last_ack = 65535
	peer.ack_bits = 1
	peer.note_received(0)
	assert peer.last_ack == 0
	assert peer.ack_bits & 1 != 0
	peer.note_received(65535)
	assert peer.ack_bits & (1 << 1) != 0
	peer.sequence = 65535
	peer.adopt_sequence(0)
	assert peer.sequence == 0
}

fn test_snapshot_negative_count_and_sorted_sample() {
	mut s := new_snapshot(0, -5)
	assert s.positions.len == 0
	assert s.velocities.len == 0
	mut buf := new_snapshot_buffer(4)
	buf.push(new_snapshot(2, 1))
	buf.push(new_snapshot(0, 1))
	buf.push(new_snapshot(1, 1))
	assert buf.items[0].time == 0
	assert buf.items[1].time == 1
	assert buf.items[2].time == 2
	if first, second, t := buf.sample(1.5) {
		assert first.time == 1 && second.time == 2
		assert t > 0.49 && t < 0.51
	} else {
		assert false, 'sample should find the two adjacent snapshots'
	}
}
