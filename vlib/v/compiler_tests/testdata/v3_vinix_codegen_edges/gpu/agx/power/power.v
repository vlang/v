module power

pub struct Segment {
	flags u32
}

pub fn (segment &Segment) is_writable() bool {
	return segment.flags & 1 == 0
}

pub fn fixed_receiver() bool {
	segments := [Segment{ flags: 1 }, Segment{ flags: 0 }]!
	return segments[0].is_writable() || segments[1].is_writable()
}
