module strconv

// atof64 return a f64 from a string doing a parsing operation
pub fn atof64(s string) f64 {
	res := 0.0
	#res.val = Number(s.str)

	return res
}
