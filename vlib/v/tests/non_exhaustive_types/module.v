module non_exhaustive_types

@[non_exhaustive]
pub enum Status {
	ready
	running
	failed
}

pub struct Connected {
pub:
	x int
}

pub struct Disconnected {
pub:
	y int
}

pub struct MessageReceived {
pub:
	z int
}

@[non_exhaustive]
pub type Event = Connected | Disconnected | MessageReceived
