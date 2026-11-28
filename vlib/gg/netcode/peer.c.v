module netcode

import net

// Peer is a native UDP endpoint. Each datagram carries a sequence number,
// an acknowledgement bitfield and a Packet payload.
pub struct Peer {
mut:
	conn     &net.UdpConn
	sequence u16
	// last_ack is the highest sequence number this peer has actually received;
	// ack_bits marks the 32 sequences at and before it that were received.
	last_ack u16
	ack_bits u32
	// A listening socket has no configured remote. Remember the most recent
	// sender so replies can be routed back with write_to.
	sender     net.Addr
	has_sender bool
}

// listen binds a UDP socket to addr, for example '0.0.0.0:7777'.
pub fn listen(addr string) !&Peer {
	conn := net.listen_udp(addr)!
	return &Peer{ conn: conn }
}

// connect creates a UDP socket bound to the given remote address.
pub fn connect(addr string) !&Peer {
	conn := net.dial_udp(addr)!
	return &Peer{ conn: conn }
}

// send writes a sequenced datagram. The 8-byte header is seq:u16,
// last_ack:u16, ack_bits:u32. Sockets with no configured remote reply to the
// last sender that recv() saw; if none is known, an error is returned.
pub fn (mut p Peer) send(payload Packet) ! {
	mut wire := new_packet()
	p.sequence++
	wire.write_u16(p.sequence)
	wire.write_u16(p.last_ack)
	wire.write_u32(p.ack_bits)
	body := payload.bytes()
	wire.data << body
	if p.conn.sock.remote() != none {
		p.conn.write(wire.data)!
	} else if p.has_sender {
		p.conn.write_to(p.sender, wire.data)!
	} else {
		return error('netcode: no remote address known for peer')
	}
}

// note_received updates the acknowledgement window from a sequence number
// this peer has actually received. Diffs are computed modulo 2^16 so the
// u16 sequence wrapping from 65535 to 0 is handled correctly.
fn (mut p Peer) note_received(seq u16) {
	diff := i16(seq - p.last_ack)
	if diff > 0 {
		shift := u32(diff)
		p.ack_bits = if shift >= 32 { 1 } else { (p.ack_bits << shift) | 1 }
		p.last_ack = seq
	} else if diff < 0 {
		age := u32(-i64(diff))
		if age < 32 {
			p.ack_bits |= u32(1) << age
		}
	} else {
		p.ack_bits |= 1
	}
}

// adopt_sequence moves the outbound sequence forward with wrap-aware
// comparison; a plain `seq > p.sequence` would stall after 65535 wraps.
fn (mut p Peer) adopt_sequence(seq u16) {
	if i16(seq - p.sequence) > 0 {
		p.sequence = seq
	}
}

// decode_header splits an 8-byte datagram header and returns seq, last_ack,
// ack_bits and the remaining payload bytes.
pub fn decode_header(data []u8) !(u16, u16, u32, []u8) {
	if data.len < 8 {
		return error('netcode: datagram too short')
	}
	mut wire := from_bytes(data)
	seq := wire.read_u16()!
	last_ack := wire.read_u16()!
	ack_bits := wire.read_u32()!
	return seq, last_ack, ack_bits, wire.data[wire.pos..].clone()
}

// recv reads one datagram and returns its payload after the header.
pub fn (mut p Peer) recv() !Packet {
	mut buf := []u8{len: 4096}
	n, sender := p.conn.read(mut buf)!
	seq, _, _, body := decode_header(buf[..n])!
	p.sender = sender
	p.has_sender = true
	// Acknowledgements describe what WE received, not what the sender claims
	// it received in its own header.
	p.note_received(seq)
	p.adopt_sequence(seq)
	mut payload := new_packet()
	for b in body {
		payload.write_u8(b)
	}
	return payload
}

pub fn (mut p Peer) close() ! {
	p.conn.close()!
}

pub fn (p &Peer) sequence() u16 {
	return p.sequence
}
