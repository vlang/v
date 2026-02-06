module big

pub const zero_int = Integer{
	digits:   []u64{len: 0}
	signum:   0
	is_const: true
}
pub const one_int = Integer{
	digits:   [u64(1)]
	signum:   1
	is_const: true
}
pub const two_int = Integer{
	digits:   [u64(2)]
	signum:   1
	is_const: true
}
pub const three_int = Integer{
	digits:   [u64(3)]
	signum:   1
	is_const: true
}
