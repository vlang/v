module strconv

// atof64 return a f64 from a string doing a parsing operation
pub fn atof64(s string) ?f64 {
	// TODO: handle parsing invalid numbers as close as possible to the pure V version
	// that may be slower, but more portable, and will guarantee that higher level code
	// works the same in the JS version, as well as in the C and Native versions.
	res := 0.0
	#res.val = Number(s.str)

	return res
}
