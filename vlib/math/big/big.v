module big

pub const (
	zero_int = Integer{
		digits: []u32{len: 0}
		signum: 0
		is_const: true
	}
	one_int = Integer{
		digits: [u32(1)]
		signum: 1
		is_const: true
	}
	two_int = Integer{
		digits: [u32(2)]
		signum: 1
		is_const: true
	}
	three_int = Integer{
		digits: [u32(3)]
		signum: 1
		is_const: true
	}
)
