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
