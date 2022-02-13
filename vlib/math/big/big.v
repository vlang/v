module big

pub const (
	zero_int = Integer{
		digits: []u32{len: 0}
		signum: 0
	}
	one_int = Integer{
		digits: [u32(1)]
		signum: 1
	}
	two_int = Integer{
		digits: [u32(2)]
		signum: 1
	}
)
