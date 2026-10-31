module enumhelperdecoy

pub type Word = u16

@[comptime]
pub fn offset(value int) int {
	return value + 100
}
