module amod

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
