module big

pub const zero_int = Integer{
	digits:   []u64{len: 0}
	signum:   0
	is_const: true
}
pub const one_int = positive_integer(1)
pub const two_int = positive_integer(2)
pub const three_int = positive_integer(3)

pub const c0 = zero_int
pub const c1 = positive_integer(1)
pub const c2 = positive_integer(2)
pub const c3 = positive_integer(3)
pub const c4 = positive_integer(4)
pub const c5 = positive_integer(5)
pub const c6 = positive_integer(6)
pub const c7 = positive_integer(7)
pub const c8 = positive_integer(8)
pub const c9 = positive_integer(9)
pub const c10 = positive_integer(10)
pub const c11 = positive_integer(11)
pub const c12 = positive_integer(12)
pub const c13 = positive_integer(13)
pub const c14 = positive_integer(14)
pub const c15 = positive_integer(15)
pub const c16 = positive_integer(16)
pub const c17 = positive_integer(17)
pub const c18 = positive_integer(18)
pub const c19 = positive_integer(19)
pub const c20 = positive_integer(20)

fn positive_integer(x int) Integer {
	return Integer{
		digits:   [u64(x)]
		signum:   1
		is_const: true
	}
}
