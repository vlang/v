module ifacepkt

// sumpkt declares a sum type with the same short name as Packet.
pub interface Packet {
	id() int
}

// Frame has no homonym in any imported module.
pub interface Frame {
	id() int
}

pub struct Ping {}

pub fn (p &Ping) id() int {
	return 1
}

pub struct Pong {
	n int
}

pub fn (p Pong) id() int {
	return p.n
}

pub fn from_closure() Packet {
	f := fn () Packet {
		return Packet(&Ping{})
	}
	return f()
}

pub fn from_value(n int) Packet {
	return Packet(Pong{n})
}

pub fn from_local() int {
	p := Packet(&Ping{})
	return p.id()
}

pub fn frame() Frame {
	return Frame(&Ping{})
}
