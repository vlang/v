module bitset27465

pub struct BitSet[T] {
pub mut:
	bits  T
	index int = -1
}

// set shares its name with the alias method `Candidates.set` defined in the
// importing module. Before #27465 was fixed, that name collision made the call
// site of the alias method emit a bogus generic suffix (`..._set_T_u32`),
// referencing an undefined C symbol.
pub fn (mut b BitSet[T]) set(bit T) {
	b.bits |= T(1) << bit
}

pub fn (mut b BitSet[T]) set_all() {
	b.bits = ~T(0)
}
