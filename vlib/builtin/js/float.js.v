module builtin

pub fn (x f32) str() string {
	res := ''
	#res.str = x.val + ''

	return res
}

pub fn (x f64) str() string {
	res := ''
	#res.str = x.val + ''

	return res
}
