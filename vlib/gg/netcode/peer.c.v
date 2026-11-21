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

// send writes a sequenced datagram. seq/ack are stored in the first 6 bytes.
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

// recv reads one datagram and returns its payload after the header.
pub fn (mut p Peer) recv() !Packet {
	mut buf := []u8{len: 4096}
	n, _ := p.conn.read(mut buf)!
	if n < 6 {
		return error('netcode: datagram too short')
	}
	mut wire := from_bytes(buf[..n])
	seq := wire.read_u16()!
	p.last_ack = seq
	if seq > p.sequence {
		p.sequence = seq
	}
	p.ack_bits = wire.read_u32()!
	_ = wire.read_u16()! // ack field from sender
	mut payload := new_packet()
	for wire.remaining() > 0 {
		payload.write_u8(wire.read_u8()!)
	}
	return payload
}

pub fn (mut p Peer) close() ! {
	p.conn.close()!
}

pub fn (p &Peer) sequence() u16 {
	return p.sequence
}
