module math

pub fn inf(sign int) f64 {
	mut res := 0.0
	if sign >= 0 {
		#res.val = Infinity
	} else {
		#res.val = -Infinity
	}
	return res
}

pub fn nan() f64 {
	mut res := 0.0
	#res.val = NaN

	return res
}
