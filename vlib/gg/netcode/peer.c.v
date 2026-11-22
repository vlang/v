module netcode

import net

// Peer is a native UDP endpoint. Each datagram carries a sequence number,
// an acknowledgement bitfield and a Packet payload.
pub struct Peer {
mut:
	conn     &net.UdpConn
	sequence u16
	last_ack u16
	ack_bits u32
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
// last_ack:u16, ack_bits:u32.
pub fn (mut p Peer) send(payload Packet) ! {
	mut wire := new_packet()
	p.sequence++
	wire.write_u16(p.sequence)
	wire.write_u16(p.last_ack)
	wire.write_u32(p.ack_bits)
	body := payload.bytes()
	for b in body {
		wire.write_u8(b)
	}
	p.conn.write(wire.bytes())!
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
	n, _ := p.conn.read(mut buf)!
	seq, sender_ack, bits, body := decode_header(buf[..n])!
	p.last_ack = sender_ack
	p.ack_bits = bits
	if seq > p.sequence {
		p.sequence = seq
	}
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
