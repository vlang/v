module builtin

// tos creates a V string, given a C style pointer to a 0 terminated block.
// Note: the memory block pointed by s is *reused, not copied*!
// It will panic, when the pointer `s` is 0.
// See also `tos_clone`.
@[unsafe]
pub fn tos(s &u8, len int) string {
	if s == 0 {
		panic('tos(): nil string')
	}
	return string{
		str: unsafe { s }
		len: len
	}
}
