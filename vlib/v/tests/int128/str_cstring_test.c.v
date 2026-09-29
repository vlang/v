// `C.` lives here rather than in a plain `.v` test, so that the C-string claims
// are made by a C function and not by the compiler's own view of the value.
fn test_wide_str_is_a_terminated_c_string() {
	biggest := (u128(1) << 127) - u128(1) + (u128(1) << 127)
	s := biggest.str()
	assert s.len == 39
	seen := unsafe { C.strlen(&char(s.str)) }
	assert int(seen) == s.len
	negative := (i128(-1) << 127).str()
	negative_seen := unsafe { C.strlen(&char(negative.str)) }
	assert int(negative_seen) == negative.len
}

fn test_wide_str_can_be_freed_by_its_caller() {
	// Length and terminator checks alone accept an interior pointer. Ordinary
	// string freeing must also be safe, including when V is run with `-gc none`.
	small := u128(7).str()
	positive := i128(7).str()
	biggest := ((u128(1) << 127) - u128(1) + (u128(1) << 127)).str()
	negative := i128(-7).str()
	minimum := (i128(-1) << 127).str()
	assert small == '7'
	assert positive == '7'
	assert biggest == '340282366920938463463374607431768211455'
	assert negative == '-7'
	assert minimum == '-170141183460469231731687303715884105728'
	// All five strings own their allocations; none may point inside a digit array.
	unsafe {
		small.free()
		positive.free()
		biggest.free()
		negative.free()
		minimum.free()
	}
}
