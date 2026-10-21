module sumpkt

pub struct Goodbye {}

pub struct Report {
pub:
	count int
}

pub type Packet = Goodbye | Report

pub fn report(count int) Packet {
	return Packet(Report{count})
}
